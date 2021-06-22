module WelcomeEmail.Server.Util where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log)
import Node.Process (exit)
import Simple.JSON (readJSON)

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


type User = { name :: String, pwd :: String }

getUsers :: Effect (Either String (Array User))
getUsers = do
  str <- getUsersImpl
  pure $ lmap show $ readJSON str

getUsers' :: forall m. MonadEffect m => MonadThrow String m => m (Either String (Array User))
getUsers' = do
  str <- liftEffect $ getUsersImpl
  void $ throwError "lulu"
  pure $ lmap show $ readJSON str


unwrapOrExit :: forall a b. Show a => String -> Either a b -> Effect b
unwrapOrExit msg = case _ of
  Left err -> do
    log $ msg <> ": " <> show err
    exit 1
  Right result -> pure result

-- unwrapOr :: forall a b. Show a => (a -> Effect Unit) -> Either a b -> Effect b
-- unwrapOr handler = case _ of
--   Left err -> handler err
--   Right result -> pure result


foreign import getNodeEnvImpl :: Effect String
foreign import getUsersImpl :: Effect String

foreign import dotenvConfig :: Effect Unit

foreign import tokenSecret :: String
foreign import jwtSign :: forall r1 r2. Record r1 -> String -> Record r2 -> String
