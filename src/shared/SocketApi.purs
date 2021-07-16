module WelcomeEmail.Shared.SocketApi where

import MagLibs.SocketIo.Message (ClientToServerMsg(..), ServerToClientMsg(..))
import Type.Proxy (Proxy(..))

-- Server to Client

isRunningMsg :: ServerToClientMsg (Proxy "isRunning") Boolean
isRunningMsg = ServerToClientMsg Proxy

-- Client to Server

toggleRunningMsg :: ClientToServerMsg (Proxy "toggleRunning") {}
toggleRunningMsg = ClientToServerMsg Proxy
