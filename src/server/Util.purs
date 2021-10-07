module KvmMail.Server.Util where

import ThisPrelude

import Data.Nullable (Nullable, toMaybe)
import Effect.Console (error)
import Effect.Exception (message, name, try)
import Foreign (Foreign)
import Node.Process (exit)
import Simple.JSON (readJSON, readJSON_)
import KvmMail.Server.Subscription.Entities (BBox, maxlat, maxlng, minlat, minlng)


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

foreign import getNodeEnvImpl :: Effect String

type User = { name :: String, pwd :: String }

getUsers :: Effect (Either String (Array User))
getUsers = do
  str <- getUsersImpl
  pure $ lmap show $ readJSON str

foreign import getUsersImpl :: Effect String

getHost :: Effect (Maybe String)
getHost = getHostImpl >>= pure <<< toMaybe

foreign import getHostImpl :: Effect (Nullable String)

getPort :: Effect (Maybe Int)
getPort = do
  mbstr <- getPortImpl >>= pure <<< toMaybe
  pure do
    str <- mbstr
    readJSON_ str

foreign import getPortImpl :: Effect (Nullable String)

getApiBaseUrl :: forall m. MonadEffect m => m (Maybe String)
getApiBaseUrl = liftEffect $ getApiBaseUrlImpl >>= pure <<< toMaybe

foreign import getApiBaseUrlImpl :: Effect (Nullable String)


unwrapOrExit :: forall a b. Show a => String -> Either a b -> Effect b
unwrapOrExit msg = case _ of
  Left err -> do
    log $ msg <> ": " <> show err
    exit 1
  Right result -> pure result

unwrapOrExitMb :: forall a. String -> Maybe a -> Effect a
unwrapOrExitMb msg = case _ of
  Nothing -> do
    error msg
    exit 1
  Just result -> pure result

jwtVerify :: String -> Effect (Either String Foreign)
jwtVerify token = do
  result <- try $ jwtVerifyImpl token
  pure $ lmap (\e -> name e <> ": " <> message e) result

jwtVerifyS :: String -> String -> Effect (Either String Foreign)
jwtVerifyS token secret = do
  result <- try $ jwtVerifySImpl token secret
  pure $ lmap (\e -> name e <> ": " <> message e) result

foreign import tokenSecret :: String
foreign import jwtSign :: forall r1 r2. Record r1 -> String -> Record r2 -> String
foreign import jwtVerifyImpl :: String -> Effect Foreign
foreign import jwtVerifySImpl :: String -> String -> Effect Foreign
foreign import jwtDecode :: String -> Foreign

-- turf

foreign import isInAt :: forall r. { lat :: Number, lng :: Number | r } -> Boolean
foreign import isInDe :: forall r. { lat :: Number, lng :: Number | r } -> Boolean
foreign import isInCh :: forall r. { lat :: Number, lng :: Number | r } -> Boolean

isInDach :: forall r. { lat :: Number, lng :: Number | r } -> Boolean
isInDach p = isInDe p || isInAt p || isInCh p

foreign import isInBBoxImpl :: forall r. { lat :: Number, lng :: Number | r } -> Array Number -> Boolean
isInBBox :: forall r. BBox -> { lat :: Number, lng :: Number | r } -> Boolean
isInBBox bb p = isInBBoxImpl p [minlng bb, minlat bb, maxlng bb, maxlat bb]

