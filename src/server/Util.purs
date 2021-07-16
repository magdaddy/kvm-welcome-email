module WelcomeEmail.Server.Util where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log)
import Effect.Exception (message, name, try)
import Foreign (Foreign)
import Node.Process (exit)
import Simple.JSON (readJSON)


foreign import dotenvConfig :: Effect Unit

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

foreign import getNodeEnvImpl :: Effect String
foreign import getUsersImpl :: Effect String


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

jwtVerify :: String -> Effect (Either String Foreign)
jwtVerify token = do
  result <- try $ jwtVerifyImpl token
  pure $ lmap (\e -> name e <> ": " <> message e) result

foreign import tokenSecret :: String
foreign import jwtSign :: forall r1 r2. Record r1 -> String -> Record r2 -> String
foreign import jwtVerifyImpl :: String -> Effect Foreign

-- turf

foreign import isInAt :: forall r. { lat :: Number, lng :: Number | r } -> Boolean
foreign import isInDe :: forall r. { lat :: Number, lng :: Number | r } -> Boolean
foreign import isInCh :: forall r. { lat :: Number, lng :: Number | r } -> Boolean

isInDach :: forall r. { lat :: Number, lng :: Number | r } -> Boolean
isInDach p = isInDe p || isInAt p || isInCh p
