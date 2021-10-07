module KvmMail.App.LoginPage where

import ThisPrelude

import Control.Monad.Except (runExceptT)
import Data.Maybe (fromMaybe)
import Data.Tuple.Nested ((/\))
import Effect.Aff (launchAff_)
import React.Basic.DOM as R
import React.Basic.DOM.Events (targetValue)
import React.Basic.Events (handler, handler_)
import React.Basic.Hooks (Component, component, useState, useState')
import React.Basic.Hooks as React
import KvmMail.App.Api.Backend (login)
import KvmMail.App.Api.Web (saveTokenToLocalStorage)
import KvmMail.App.Data (Page(..))
import KvmMail.Shared.Boundary (LoginData)


mkLoginPage :: Component { setPage :: Page -> Effect Unit }
mkLoginPage = do
  component "LoginPage" \props -> React.do
    (loginData :: LoginData) /\ setLoginData <- useState { username: "", password: "" }
    (errMsg :: String) /\ setErrMsg <- useState' ""

    let
      submit = launchAff_ do
        response <- runExceptT $ login loginData
        case response of
          Left err -> liftEffect $ setErrMsg $ show err
          Right token -> do
            liftEffect $ saveTokenToLocalStorage token
            liftEffect $ props.setPage StatusPage

      field label input = R.div
        { className: "field"
        , children:
          [ R.label { className: "label", children: [ R.text label ] }
          , R.div { className: "control", children: [ input ] }
          ]
        }
    pure $
      R.div
        { className: "page login container is-max-desktop"
        , children:
          [ R.div
              { className: "box mt-5"
              , children:
                [ R.text "Login"
                , R.div_
                    [ field "Username: " $ R.input
                        { className: "input"
                        , value: loginData.username
                        , onChange: handler targetValue \val -> do
                            setLoginData _ { username = fromMaybe "" val }
                            setErrMsg ""
                        }
                    , field "Password: " $ R.input
                        { className: "input"
                        , type: "password"
                        , value: loginData.password
                        , onChange: handler targetValue \val -> do
                            setLoginData _ { password = fromMaybe "" val }
                            setErrMsg ""
                        }
                    , R.button
                        { className: "button is-primary"
                        , children: [ R.text "Login" ]
                        , onClick: handler_ submit
                        }
                    , R.text errMsg
                    ]
                ]
              }
          ]
        }


