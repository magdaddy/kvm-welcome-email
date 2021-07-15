module WelcomeEmail.App.TestMail where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple.Nested ((/\))
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Network.RemoteData (RemoteData(..))
import React.Basic.DOM as R
import React.Basic.DOM.Events (targetValue)
import React.Basic.Events (handler, handler_)
import React.Basic.Hooks (Component, component, useState')
import React.Basic.Hooks as React
import WelcomeEmail.App.Api.Backend as Api


mkTestMailComponent :: Component {}
mkTestMailComponent = do
  component "TestMailComponent" \_props -> React.do
    (emailAddr :: String) /\ setEmailAddr <- useState' ""
    (result :: RemoteData String Unit) /\ setResult <- useState' NotAsked

    let
      sendHandler = launchAff_ do
        liftEffect $ setResult Loading
        res <- Api.sendTestMail { emailAddr }
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

-- newtype EmailAddrForm (r :: Row Type -> Type) f = EmailAddrForm (r
--   ( emailAddr :: f Void String String
--   ))
-- derive instance newtypeEmailAddrForm :: Newtype (EmailAddrForm r f) _

-- -- type State = { result :: RemoteData String Unit }

-- data Action
--   = Send TestMailPayload

-- component' :: forall q m.
--   MonadAff m =>
--   ManageSettings m =>
--   SendTestMail m =>
--   H.Component q Unit Void m
-- component' = H.mkComponent
--   { initialState: \_ -> { result: NotAsked }
--   , render
--   , eval: H.mkEval $ H.defaultEval { handleAction = handleAction, initialize = Nothing }
--   }
--   where
--   handleAction = case _ of
--     Send pl -> do
--       H.modify_ _ { result = Loading }
--       result <- sendTestMail pl
--       case result of
--         Left err -> H.modify_ _ { result = Failure $ show err }
--         Right response -> case response.error of
--           Nothing -> H.modify_ _ { result = Success unit }
--           Just err -> H.modify_ _ { result = Failure $ show err }

--   render state = HH.div_
--     [ HH.slot F._formless unit formComponent { emailAddr: "" } Send
--     , case state.result of
--         NotAsked -> HH.text ""
--         Loading -> HH.text "Sending test mail..."
--         Failure err -> HH.text err
--         Success _ -> HH.text "Test mail sent successfully."
--     ]

--   formComponent :: F.Component EmailAddrForm (Const Void) () TestMailPayload TestMailPayload m
--   formComponent = F.component formInput $ F.defaultSpec { render = renderFormless, handleEvent = F.raiseResult }
--     where
--     formInput inp =
--       { validators: EmailAddrForm
--         { emailAddr: F.noValidation
--         }
--       , initialInputs: Just (F.wrapInputFields inp)
--       }

--     renderFormless st =
--       HH.div [ cls "field has-addons" ]
--         [ HH.p [ cls "control" ]
--             [ HH.button
--                 [ cls "button is-primary", HE.onClick \_ -> F.submit ]
--                 [ HH.text "Send test mail to" ]
--             ]
--         , HH.p [ cls "control is-expanded" ]
--             [ HH.input
--                 [ cls "input", HP.value $ F.getInput _emailAddr st.form
--                 , HE.onValueInput (F.set _emailAddr)
--                 ]
--             ]
--         ]
--       where
--       _emailAddr = Proxy :: _ "emailAddr"

-- strIsInt :: ∀ form m. Monad m => Validation form m String String Int
-- strIsInt = hoistFnE_ $ \str -> maybe (Left $ "InvalidInt" <> str) Right (Int.fromString str)

