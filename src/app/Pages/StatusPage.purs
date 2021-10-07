module KvmMail.App.StatusPage where

import ThisPrelude

import Data.Tuple.Nested ((/\))
import Effect.Aff (launchAff_)
import MagLibs.DateFns (format)
import Network.RemoteData (RemoteData(..), fromEither, toMaybe)
import React.Basic.DOM (css)
import React.Basic.DOM as R
import React.Basic.Events (handler_)
import React.Basic.Hooks (Component, JSX, component, fragment, useEffect, useEffectOnce, useState')
import React.Basic.Hooks as React
import KvmMail.App.Api.Backend as Api
import KvmMail.App.Data (AppError(..), Page(..))
import KvmMail.Shared.Boundary (LastLogEntry, LastLogType(..))
import KvmMail.Shared.Template (entryLink)


mkStatusPage :: Component { setPage :: Page -> Effect Unit }
mkStatusPage = do
  component "StatusPage" \props -> React.do
    (isRunning :: RemoteData AppError Boolean) /\ setIsRunning <- useState' NotAsked
    (lastLogs :: RemoteData AppError (Array LastLogEntry)) /\ setLastLogs <- useState' NotAsked

    let
      toggleRunning = launchAff_ do
        liftEffect $ setIsRunning Loading
        result <- runExceptT Api.toggleRunning
        liftEffect $ setIsRunning $ fromEither $ rmap _.isRunning result

    useEffectOnce do
      launchAff_ do
        liftEffect $ setIsRunning Loading
        result <- runExceptT Api.serverState
        case result of
          Left (Unauthorized err) -> do
            liftEffect $ log $ "Unauthorized: " <> err
            liftEffect $ props.setPage LoginPage
          _ -> liftEffect $ setIsRunning $ fromEither $ rmap _.isRunning result
      pure mempty

    useEffect (toMaybe isRunning) do
      launchAff_ do
        liftEffect $ setLastLogs Loading
        resLastLogs <- runExceptT Api.getLastLogs
        liftEffect $ setLastLogs $ fromEither resLastLogs
      pure mempty

    pure $
      R.div
        { className: "page settings container is-max-desktop"
        , children:
          [ R.div
              { className: "box mt-5"
              , children:
                [ case isRunning of
                    NotAsked -> R.text "Not asked"
                    Loading -> R.text "Loading..."
                    Failure e -> R.text $ show e
                    Success isRunning' -> do
                      let bclass = if isRunning' then "is-primary" else "is-primary"
                      let statusText = if isRunning' then "running" else "not running"
                      let bgClass = if isRunning' then "has-background-success-dark" else "has-background-danger-dark"
                      R.div
                        { className: "is-flex is-align-items-center"
                        , children:
                          [ R.span
                              { className: "mr-2"
                              , style: css { display: "inline-block" }
                              , children: [ R.text $ "Status: " <> statusText ]
                              }
                          , R.span
                              { className: bgClass <> " mr-2"
                              , style: css { height: "2rem", width: "2rem", borderRadius: "50%", display: "inline-block" }
                              }
                          , R.button
                              { className: "button " <> bclass
                              , children: [ R.text if isRunning' then "Stop" else "Start" ]
                              , onClick: handler_ toggleRunning
                              }
                          ]
                        }
                , R.h1_ [ R.text "Last logs" ]
                , case lastLogs of
                    NotAsked -> R.text "Not asked"
                    Loading -> R.text "Loading..."
                    Failure e -> R.text $ show e
                    Success lastLogs' -> do
                      R.div
                        { className: ""
                        , style: css { height: "30rem", overflowY: "scroll" }
                        , children: map renderLastLogEntry lastLogs'
                        }
                ]
              }
          ]
        }


renderLastLogEntry :: LastLogEntry -> JSX
renderLastLogEntry lle = R.div
    { className: cN
    , children:
      [ R.span
          { className: "is-family-monospace mr-2"
          , children: [ R.text $ format "yy-MM-dd HH:mm:ss" lle.timestamp ]
          }
      , case lle.type of
          Error msg -> R.text $ " Error: " <> msg
          Warn msg -> R.span { children: [ R.text msg ] }
          EmailSent entry -> fragment
            [ R.text $ "Sent to "
            , R.a
                { href: entryLink entry
                , target: "_blank"
                , rel: "noopener noreferrer"
                , children: [ R.text entry.title ]
                }
            ]
      ]
    }
  where
  cN = case lle.type of
    Error _ -> "has-text-danger-dark"
    Warn _ -> "has-text-warning-dark"
    EmailSent _ -> ""
