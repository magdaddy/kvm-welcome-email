module WelcomeEmail.App.LoginForm where

import Prelude

import Data.Maybe (fromMaybe)
import Data.Tuple.Nested ((/\))
import Effect.Aff (launchAff_)
import React.Basic.DOM as R
import React.Basic.DOM.Events (targetValue)
import React.Basic.Events (handler, handler_)
import React.Basic.Hooks (Component, component, useState)
import React.Basic.Hooks as React
import WelcomeEmail.App.Api.Backend as Api
import WelcomeEmail.Shared.Boundary (LoginData)


mkFormComponent :: Component
  { --loginData :: LoginData
  }
mkFormComponent = do
  component "FormComponent" \_props -> React.do
    -- (loginData :: LoginData) /\ setLoginData <- useState props.loginData
    (loginData :: LoginData) /\ setLoginData <- useState { username: "", password: "" }

    let
      submit = launchAff_ do
        pure unit
            -- result <- Api.saveSettings s
            -- case result of
            --   Left err -> liftEffect $ logShow err
            --   Right _ -> do
            --     liftEffect $ props.setSettings s
            --     liftEffect $ props.setEdit false

      field label input = R.div
        { className: "field"
        , children:
          [ R.label { className: "label", children: [ R.text label ] }
          , R.div { className: "control", children: [ input ] }
          ]
        }

    pure $
      R.div_
        [ field "Username: " $ R.input
            { className: "input"
            , value: loginData.username
            , onChange: handler targetValue \val ->
                setLoginData _ { username = fromMaybe "" val }
            }
        , field "Password: " $ R.input
            { className: "input"
            , value: loginData.password
            , onChange: handler targetValue \val ->
                setLoginData _ { password = fromMaybe "" val }
            }
        , R.button
            { className: "button is-primary"
            , children: [ R.text "Login" ]
            , onClick: handler_ submit
            }
        ]


-- newtype LoginForm (r :: Row Type -> Type) f = LoginForm (r
--   ( username :: f Void String String
--   , password :: f Void String String
--   ))
-- derive instance newtypeLoginForm :: Newtype (LoginForm r f) _

-- data Action
--   = Login LoginData

-- component' :: forall q m.
--   MonadAff m =>
--   H.Component q Unit Void m
-- component' = H.mkComponent
--   { initialState: \_ -> { result: NotAsked }
--   , render
--   , eval: H.mkEval $ H.defaultEval { handleAction = handleAction, initialize = Nothing }
--   }
--   where
--   handleAction = case _ of
--     Login loginData -> do
--       mempty

--   render state = HH.div_
--     [ HH.slot F._formless unit formComponent { username: "", password: "" } Login
--     , case state.result of
--         NotAsked -> HH.text ""
--         Loading -> HH.text "Sending test mail..."
--         Failure err -> HH.text err
--         Success _ -> HH.text "Test mail sent successfully."
--     ]

--   formComponent :: F.Component LoginForm (Const Void) () LoginData LoginData m
--   formComponent = F.component formInput $ F.defaultSpec { render = renderFormless, handleEvent = F.raiseResult }
--     where
--     formInput inp =
--       { validators: LoginForm
--         { username: F.noValidation
--         , password: F.noValidation
--         }
--       , initialInputs: Just (F.wrapInputFields inp)
--       }

--     renderFormless st =
--       HH.div_
--         [ field "Username: " $ HH.input
--             [ cls "input", HP.value $ F.getInput _username st.form
--             , HE.onValueInput (F.set _username)
--             ]
--         , field "Password: " $ HH.input
--             [ cls "input", HP.value $ F.getInput _password st.form
--             , HE.onValueInput (F.set _password)
--             ]
--         , HH.button
--             [ cls "button is-primary", HE.onClick \_ -> F.submit ]
--             [ HH.text "Login" ]
--         ]
--       where
--       _username = Proxy :: _ "username"
--       _password = Proxy :: _ "password"
--       field label input = HH.div [ cls "field" ]
--         [ HH.label [ cls "label" ] [ HH.text label ]
--         , HH.div [ cls "control" ] [ input ]
--         ]

