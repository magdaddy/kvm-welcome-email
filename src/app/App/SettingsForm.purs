module WelcomeEmail.App.SettingsForm where

import Prelude

import Data.Either (Either(..))
import Data.Int (fromString) as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (null)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log, logShow)
import Network.RemoteData (RemoteData(..), fromEither)
import React.Basic.DOM as R
import React.Basic.DOM.Events (targetValue)
import React.Basic.Events (handler, handler_)
import React.Basic.Hooks (Component, component, useEffectOnce, useState, useState')
import React.Basic.Hooks as React
import WelcomeEmail.App.Api as Api
import WelcomeEmail.App.Data (AppError)
import WelcomeEmail.App.TestMail (mkTestMailComponent)
import WelcomeEmail.Shared.Boundary (Settings)


mkSettingsForm :: Component {}
mkSettingsForm = do
  formComponent <- mkFormComponent
  testMailComponent <- mkTestMailComponent
  component "SettingsForm" \_props -> React.do
    (settings :: RemoteData AppError Settings) /\ setSettings <- useState' NotAsked
    (edit :: Boolean) /\ setEdit <- useState' false

    let
      field label input = R.div
        { className: "field"
        , children:
          [ R.label { className: "label", children: [ R.text label ] }
          , R.div { className: "control", children: [ input ] }
          ]
        }
      showEmpty str = if null str then "---" else str

    useEffectOnce do
      launchAff_ do
        liftEffect $ setSettings Loading
        result <- Api.getSettings
        liftEffect $ setSettings $ fromEither result
      pure mempty

    pure $
      R.section
        { className: "box"
        , children:
          [ case settings of
              NotAsked -> R.text "not asked"
              Loading -> R.text "loading"
              Failure err -> R.text $ show err
              Success settings' ->
                if edit then
                  R.div_
                    [ R.div
                        { className: "has-text-right"
                        , children:
                          [ R.button
                              { className: "button is-light"
                              , children: [ R.text "Cancel" ]
                              , onClick: handler_ $ setEdit false
                              }
                          ]
                        }
                    , formComponent
                        { localSettings: toLocal settings'
                        , settings: settings'
                        , setSettings: \s -> setSettings $ Success s
                        , setEdit
                        }
                    ]
                else
                  R.div_
                    [
                      R.div
                        { className: "has-text-right"
                        , children:
                          [ R.button
                              { className: "button is-primary"
                              , children: [ R.text "Edit" ]
                              , onClick: handler_ $ setEdit true
                              }
                          ]
                        }
                    -- , R.text $ show settings'
                    , field "Sender address" $ R.text $ showEmpty settings'.senderAddress
                    , field "Username" $ R.text $ showEmpty settings'.nodeMailer.auth.user
                    , field "Password" $ R.text $ showEmpty settings'.nodeMailer.auth.pass
                    , field "Host" $ R.text $ showEmpty settings'.nodeMailer.host
                    , field "Port" $ R.text $ show settings'.nodeMailer.port
                    , R.hr {}
                    , testMailComponent {}
                    ]
          ]
        }


mkFormComponent :: Component
  { localSettings :: LocalSettings
  , settings :: Settings
  , setEdit :: Boolean -> Effect Unit
  , setSettings :: Settings -> Effect Unit
  }
mkFormComponent = do
  component "FormComponent" \props -> React.do
    (localSettings :: LocalSettings) /\ setLocalSettings <- useState props.localSettings

    let
      submit = launchAff_ do
        let mbs = updWithLocal localSettings props.settings
        case mbs of
          Nothing -> liftEffect $ log "Port is not a number"
          Just s -> do
            result <- Api.saveSettings s
            case result of
              Left err -> liftEffect $ logShow err
              Right _ -> do
                liftEffect $ props.setSettings s
                liftEffect $ props.setEdit false

      field label input = R.div
        { className: "field"
        , children:
          [ R.label { className: "label", children: [ R.text label ] }
          , R.div { className: "control", children: [ input ] }
          ]
        }

    pure $
      R.div_
        [ field "Sender address: " $ R.input
            { className: "input"
            , value: localSettings.senderAddress
            , onChange: handler targetValue \val ->
                setLocalSettings _ { senderAddress = fromMaybe "" val }
            }
        , field "Username: " $ R.input
            { className: "input"
            , value: localSettings.username
            , onChange: handler targetValue \val ->
                setLocalSettings _ { username = fromMaybe "" val }
            }
        , field "Password: " $ R.input
            { className: "input"
            , value: localSettings.password
            , onChange: handler targetValue \val ->
                setLocalSettings _ { password = fromMaybe "" val }
            }
        , field "Host: " $ R.input
            { className: "input"
            , value: localSettings.host
            , onChange: handler targetValue \val ->
                setLocalSettings _ { host = fromMaybe "" val }
            }
        , field "Port: " $ R.input
            { className: "input"
            , value: localSettings.port
            , onChange: handler targetValue \val ->
                setLocalSettings _ { port = fromMaybe "" val }
            }
        , R.button
            { className: "button is-primary"
            , children: [ R.text "Save" ]
            , onClick: handler_ submit
            }
        ]




type LocalSettings =
  { senderAddress :: String
  , username :: String
  , password :: String
  , host :: String
  -- , port :: Int
  , port :: String
  }



-- component' :: forall q m.
--   MonadAff m =>
--   ManageSettings m =>
--   SendTestMail m =>
--   H.Component q Unit Void m
-- component' = H.mkComponent
--   { initialState: \_ -> { edit: false, settings: (NotAsked :: RemoteData AppError Settings) }
--   , render
--   , eval: H.mkEval $ H.defaultEval { handleAction = handleAction, initialize = Just GetSettings }
--   }
--   where
--   handleAction = case _ of
--     Submit ls -> do
--       H.liftEffect $ logShow ls
--       -- st <- H.get
--       -- case preview (_settings <<< _Success) st of
--       --   Nothing -> H.liftEffect $ logShow "This can't happen"
--       --   Just s -> do
--       --     let ss = updWithLocal ls s
--       --     H.modify_ \st' -> set (_settings <<< _Success) ss st'
--       --     result <- saveSettings ss
--       --     case result of
--       --       Left err -> H.liftEffect $ logShow err
--       --       Right _ ->  H.modify_ _ { edit = false }
--     GetSettings -> do
--       H.modify_ _ { settings = Loading }
--       response <- getSettings
--       H.modify_ _ { settings = (fromEither response) }
--     EditClicked -> do
--       H.modify_ _ { edit = true }
--     CancelClicked -> do
--       H.modify_ _ { edit = false }

--   render state =
--     HH.section [ cls "box" ]
--       [ --HH.h1_ [ HH.text "Mailer" ]
--         if state.edit then
--           case state.settings of
--             Success settings -> HH.div_
--               [ HH.div [ cls "has-text-right" ]
--                   [ HH.button
--                       [ cls "button is-light", HE.onClick \_ -> CancelClicked ]
--                       [ HH.text "Cancel" ]
--                   ]
--               -- , HH.slot F._formless unit formComponent (toLocal settings) Submit
--               ]

--             _ -> HH.text "This should never happen"
--         else
--           case state.settings of
--             NotAsked -> HH.text "not asked"
--             Loading -> HH.text "loading"
--             Failure err -> HH.text $ show err
--             Success settings -> HH.div [ cls "" ]
--               [ HH.div [ cls "has-text-right"]
--                   [ HH.button
--                       [ cls "button is-primary", HE.onClick \_ -> EditClicked ]
--                       [ HH.text "Edit" ]
--                   ]
--               -- , HH.text $ show settings
--               , field "Sender address" $ HH.text $ showEmpty settings.senderAddress
--               , field "Username" $ HH.text $ showEmpty settings.nodeMailer.auth.user
--               , field "Password" $ HH.text $ showEmpty settings.nodeMailer.auth.pass
--               , field "Host" $ HH.text $ showEmpty settings.nodeMailer.host
--               , field "Port" $ HH.text $ show settings.nodeMailer.port
--               , HH.hr_
--               , HH.slot_ (Proxy :: _ "testMail") 0 TestMail.component unit
--               ]
--       ]
--     where
--     field label input = HH.div [ cls "field" ]
--       [ HH.label [ cls "label" ] [ HH.text label ]
--       , HH.div [ cls "control" ] [ input ]
--       ]
--     showEmpty str = if null str then "---" else str


  -- formComponent :: F.Component ContactForm (Const Void) () LocalSettings LocalSettings m
  -- formComponent = F.component formInput $ F.defaultSpec { render = renderFormless, handleEvent = F.raiseResult }
  --   where
  --   formInput inp =
  --     { validators: ContactForm
  --       { senderAddress: F.noValidation
  --       , username: F.noValidation
  --       , password: F.noValidation
  --       , host: F.noValidation
  --       , port: strIsInt
  --       }
  --     , initialInputs: Just (F.wrapInputFields (modify (Proxy :: _ "port") show inp))
  --     }

  --   renderFormless st =
  --     HH.div_ --formContent_
  --       [ field "Sender address: " $ HH.input
  --           [ cls "input", HP.value $ F.getInput _senderAddress st.form
  --           , HE.onValueInput (F.set _senderAddress)
  --           ]
  --       , field "Username: " $ HH.input
  --           [ cls "input", HP.value $ F.getInput _username st.form
  --           , HE.onValueInput (F.set _username)
  --           ]
  --       , field "Password: " $ HH.input
  --           [ cls "input", HP.value $ F.getInput _password st.form
  --           , HE.onValueInput (F.set _password)
  --           ]
  --       , field "Host: " $ HH.input
  --           [ cls "input", HP.value $ F.getInput _host st.form
  --           , HE.onValueInput (F.set _host)
  --           ]
  --       , field "Port: " $ HH.input
  --           [ cls "input", HP.value $ F.getInput _port st.form
  --           , HE.onValueInput (F.set _port)
  --           ]
  --       , HH.button
  --           [ cls "button is-primary", HE.onClick \_ -> F.submit ]
  --           [ HH.text "Save" ]
  --       ]
  --     where
  --     _senderAddress = Proxy :: _ "senderAddress"
  --     _username = Proxy :: _ "username"
  --     _password = Proxy :: _ "password"
  --     _host = Proxy :: _ "host"
  --     _port = Proxy :: _ "port"
  --     field label input = HH.div [ cls "field" ]
  --       [ HH.label [ cls "label" ] [ HH.text label ]
  --       , HH.div [ cls "control" ] [ input ]
  --       ]


-- strIsInt :: ∀ form m. Monad m => Validation form m String String Int
-- strIsInt = hoistFnE_ $ \str -> maybe (Left $ "InvalidInt" <> str) Right (Int.fromString str)

toLocal :: Settings -> LocalSettings
toLocal s =
  { senderAddress: s.senderAddress
  , username: s.nodeMailer.auth.user
  , password: s.nodeMailer.auth.pass
  , host: s.nodeMailer.host
  , port: show s.nodeMailer.port
  }

updWithLocal :: LocalSettings -> Settings -> Maybe Settings
updWithLocal ls s = do
  port <- Int.fromString ls.port
  let
    nm = s.nodeMailer
      { auth = { user: ls.username, pass: ls.password }
      , host = ls.host
      , port = port
      }
  pure $ s { nodeMailer = nm, senderAddress = ls.senderAddress }
