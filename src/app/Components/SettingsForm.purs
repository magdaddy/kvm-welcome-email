module KvmMail.App.SettingsForm where

import ThisPrelude

import Control.Monad.Except (runExceptT)
import Data.Int (fromString) as Int
import Data.Maybe (fromMaybe)
import Data.String (null)
import Data.Tuple.Nested ((/\))
import Effect.Aff (launchAff_)
import Effect.Console (logShow)
import Network.RemoteData (RemoteData(..), fromEither)
import React.Basic.DOM as R
import React.Basic.DOM.Events (targetValue)
import React.Basic.Events (handler, handler_)
import React.Basic.Hooks (Component, component, useEffectOnce, useState, useState')
import React.Basic.Hooks as React
import KvmMail.App.Api.Backend as Api
import KvmMail.App.Data (AppError(..), Page(..))
import KvmMail.App.TestMail (mkTestMailComponent)
import KvmMail.Shared.Boundary (Settings)


mkSettingsForm :: Component { setPage :: Page -> Effect Unit }
mkSettingsForm = do
  formComponent <- mkFormComponent
  testMailComponent <- mkTestMailComponent
  component "SettingsForm" \props -> React.do
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
        result <- runExceptT Api.getSettings
        case result of
          Left (Unauthorized err) -> do
            liftEffect $ log $ "Unauthorized: " <> err
            liftEffect $ props.setPage LoginPage
          _ -> liftEffect $ setSettings $ fromEither result
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
            result <- runExceptT $ Api.saveSettings s
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
