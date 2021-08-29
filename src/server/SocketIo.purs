module WelcomeEmail.Server.SocketIo where

import ThisPrelude

import Data.Maybe (isJust)
import Effect.Aff (error, forkAff, killFiber, launchAff_)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import MagLibs.SocketIo.Message (serverEmit, serverOn)
import MagLibs.SocketIo.Server (id, of_, onConnect, serverExt, socketOnDisconnect)
import Node.HTTP (Server) as Http
import WelcomeEmail.Server.Core (theloop)
import WelcomeEmail.Server.Log (LogLevel(..), logL)
import WelcomeEmail.Shared.SocketApi as SocketApi
import WelcomeEmail.Shared.State (State)

runSocketIo :: Http.Server -> Ref State -> Effect Unit
runSocketIo httpServer stateRef = do
  let options =
        { cors:
            { origin: "http://localhost:1234"
            -- { origin: "https://admin.socket.io"
            , methods: ["GET", "POST"]
            }
        }
  srv <- serverExt httpServer options
  let appNsp = of_ "/app" srv
  onConnect appNsp \skt -> do
    log $ id skt <> " connected on /app."
    socketOnDisconnect skt \reason -> do
      log $ id skt <> " left /app. " <> reason
    serverOn SocketApi.toggleRunningMsg skt \_ -> launchAff_ do
      state <- liftEffect $ Ref.read stateRef
      case state.running of
        Nothing -> do
          fiber <- forkAff $ theloop stateRef
          liftEffect $ Ref.modify_ _ { running = Just fiber } stateRef
          liftEffect $ logL Verbose "started running"
          liftEffect $ serverEmit SocketApi.isRunningMsg true appNsp
        Just fiber -> do
          killFiber (error "stop requested") fiber
          liftEffect $ Ref.modify_ _ { running = Nothing } stateRef
          liftEffect $ logL Verbose "stopped running"
          liftEffect $ serverEmit SocketApi.isRunningMsg false appNsp

    -- onAny "received on /app:" skt

    state <- Ref.read stateRef
    serverEmit SocketApi.isRunningMsg (isJust state.running) appNsp
