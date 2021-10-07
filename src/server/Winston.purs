module KvmMail.Server.Winston where

import ThisPrelude

import Node.Express.Types (Middleware)

type Info r = { level :: String, message :: String | r }

foreign import log :: forall r. Info r -> Effect Unit

foreign import morgan :: Middleware
