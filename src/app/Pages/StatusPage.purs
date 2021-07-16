module WelcomeEmail.App.StatusPage where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Console (log)
import MagLibs.SocketIo.Client (ClientSocket, connect)
import MagLibs.SocketIo.Message (clientEmit, clientOn, clientRemoveAllListenersFor)
import Network.RemoteData (RemoteData(..))
import React.Basic.DOM (css)
import React.Basic.DOM as R
import React.Basic.Events (handler_)
import React.Basic.Hooks (Component, component, useEffectOnce, useState')
import React.Basic.Hooks as React
import WelcomeEmail.App.Data (AppError, Page)
import WelcomeEmail.Shared.SocketApi as SocketApi


mkStatusPage :: Component { setPage :: Page -> Effect Unit }
mkStatusPage = do
  component "StatusPage" \_props -> React.do
    (socket :: Maybe ClientSocket) /\ setSocket <- useState' Nothing
    (isRunning :: RemoteData AppError Boolean) /\ setIsRunning <- useState' NotAsked

    let
      toggleRunning = case socket of
        Nothing -> log $ "Error: No socket."
        Just skt -> clientEmit SocketApi.toggleRunningMsg {} skt
    useEffectOnce do
      -- launchAff_ do
      --   liftEffect $ setIsRunning Loading
      --   result <- Api.serverState
      --   liftEffect $ setIsRunning $ fromEither $ rmap _.isRunning result

      skt <- connect "ws://localhost:4000/app"
      setSocket $ Just skt
      clientOn SocketApi.isRunningMsg skt \isRunning' -> do
        setIsRunning $ Success isRunning'
        log $ "isRunningMsg: " <> show isRunning'
      pure $ clientRemoveAllListenersFor SocketApi.isRunningMsg skt
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


-- render :: forall m.
--   MonadAff m =>
--   ManageSettings m =>
--   SendTestMail m =>
--   State -> H.ComponentHTML Action Slots m
-- render state =
--   HH.div [ cls "settings container is-max-desktop" ]
--     [ HH.div [ cls "box mt-5" ]
--         [ case state.isRunning of
--             NotAsked -> HH.text "Not asked"
--             Loading -> HH.text "Loading..."
--             Failure e -> HH.text $ show e
--             Success isRunning -> do
--               let bclass = if isRunning then "is-primary" else "is-primary"
--               let statusText = if isRunning then "running" else "not running"
--               let bgClass = if isRunning then "has-background-success-dark" else "has-background-danger-dark"
--               HH.div [ cls "is-flex is-align-items-center" ]
--                 [ HH.span [ cls "mr-2", HP.style "inline-block;" ]
--                     [ HH.text $ "Status: " <> statusText ]
--                 , HH.span [ cls $ bgClass <> " mr-2", HP.style "height: 2rem; width: 2rem; border-radius: 50%; display: inline-block;" ]
--                     [  ]
--                 , HH.button
--                     [ cls $ "button " <> bclass, HE.onClick \_ -> ToggleRunning ]
--                     [ HH.text if isRunning then "Stop" else "Start" ]
--                 ]
--         ]
--     ]


