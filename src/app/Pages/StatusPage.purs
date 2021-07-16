module WelcomeEmail.App.StatusPage where

import Prelude

import Data.Bifunctor (rmap)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Network.RemoteData (RemoteData(..), fromEither)
import React.Basic.DOM (css)
import React.Basic.DOM as R
import React.Basic.Events (handler_)
import React.Basic.Hooks (Component, component, useEffectOnce, useState')
import React.Basic.Hooks as React
import WelcomeEmail.App.Api.Backend as Api
import WelcomeEmail.App.Data (AppError, Page)


mkStatusPage :: Component { setPage :: Page -> Effect Unit }
mkStatusPage = do
  component "StatusPage" \_props -> React.do
    -- (socket :: Maybe ClientSocket) /\ setSocket <- useState' Nothing
    (isRunning :: RemoteData AppError Boolean) /\ setIsRunning <- useState' NotAsked

    let
      -- toggleRunning = case socket of
      --   Nothing -> log $ "Error: No socket."
      --   Just skt -> clientEmit SocketApi.toggleRunningMsg {} skt
      toggleRunning = launchAff_ do
        liftEffect $ setIsRunning Loading
        result <- Api.toggleRunning
        liftEffect $ setIsRunning $ fromEither $ rmap _.isRunning result

    useEffectOnce do
      launchAff_ do
        liftEffect $ setIsRunning Loading
        result <- Api.serverState
        liftEffect $ setIsRunning $ fromEither $ rmap _.isRunning result

      -- skt <- connect "ws://localhost:4000/app"
      -- setSocket $ Just skt
      -- clientOn SocketApi.isRunningMsg skt \isRunning' -> do
      --   setIsRunning $ Success isRunning'
      --   log $ "isRunningMsg: " <> show isRunning'
      -- pure $ clientRemoveAllListenersFor SocketApi.isRunningMsg skt
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
                ]
              }
          ]
        }


