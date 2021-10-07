module KvmMail.App.TestMail where

import ThisPrelude

import Control.Monad.Except (runExceptT)
import Data.Maybe (fromMaybe)
import Data.Tuple.Nested ((/\))
import Effect.Aff (launchAff_)
import Network.RemoteData (RemoteData(..))
import React.Basic.DOM as R
import React.Basic.DOM.Events (targetValue)
import React.Basic.Events (handler, handler_)
import React.Basic.Hooks (Component, component, useState')
import React.Basic.Hooks as React
import KvmMail.App.Api.Backend as Api


mkTestMailComponent :: Component {}
mkTestMailComponent = do
  component "TestMailComponent" \_props -> React.do
    (emailAddr :: String) /\ setEmailAddr <- useState' ""
    (result :: RemoteData String Unit) /\ setResult <- useState' NotAsked

    let
      sendHandler = launchAff_ do
        liftEffect $ setResult Loading
        res <- runExceptT $ Api.sendTestMail { emailAddr }
        case res of
          Left err -> liftEffect $ setResult $ Failure $ show err
          Right response -> case response.error of
            Nothing -> liftEffect $ setResult $ Success unit
            Just err -> liftEffect $ setResult $ Failure $ show err

    pure $
      R.div_
        [ R.div
            { className: "field has-addons"
            , children:
              [ R.p
                  { className: "control"
                  , children:
                    [ R.button
                        { className: "button is-primary"
                        , onClick: handler_ sendHandler
                        , children: [ R.text "Send test mail to" ]
                        }
                    ]
                  }
              , R.p
                  { className: "control is-expanded"
                  , children:
                    [ R.input
                        { className: "input"
                        , value: emailAddr
                        , onChange: handler targetValue \val ->
                            setEmailAddr $ fromMaybe "" val
                        }
                    ]
                  }
              ]
            }
        , case result of
            NotAsked -> R.text ""
            Loading -> R.text "Sending test mail..."
            Failure err -> R.text err
            Success _ -> R.text "Test mail sent successfully."
        ]

