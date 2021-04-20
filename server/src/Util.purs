module WelcomeEmail.Server.Util where

import Prelude

import Effect (Effect)

data NodeEnv
  = Development
  | Production
  | Other String

getNodeEnv :: Effect NodeEnv
getNodeEnv = do
  str <- getNodeEnvImpl
  pure $ case str of
    "development" -> Development
    "production" -> Production
    _ -> Other str

isOther :: NodeEnv -> Boolean
isOther (Other _) = true
isOther _ = false

foreign import getNodeEnvImpl :: Effect String

foreign import dotenvConfig :: Effect Unit
