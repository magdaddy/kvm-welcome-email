module WelcomeEmail.Server.OfdbApi where

import Prelude

import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import Data.Argonaut (Json, JsonDecodeError, decodeJson)
import Data.Array (filter, length)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.String (joinWith, null)
import Data.Tuple (Tuple, fst, snd)
import Effect (Effect)
import Effect.Aff (Aff, Error, runAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (error)
import Effect.Console (log)
import Effect.Exception (throw)
import WelcomeEmail.Shared.Entry (Entry)

throwEither :: forall a e. Show e => Either e a -> Effect a
throwEither = case _ of
  Left err -> throw $ show err
  Right x -> pure x

-- launch :: forall a. (Either Error a -> Effect Unit) -> Aff a -> Effect Unit
launch :: forall a b. Show b => (a -> b) -> Aff a -> Effect Unit
launch fun = runAff_ foo
  where
  foo eea = do
    a <- throwEither eea
    log $ show $ fun a

-- launch :: forall a. Show a => Aff a -> Effect Unit
-- launch = runAff_ (\x -> log $ show x)

ta :: forall a e. Show e => Aff (Either e a) -> Aff a
ta eaff = eaff >>= (liftEffect <<< throwEither)

x = launch (\a -> length (filter (\e -> e.version == 0) a)) $ ta getRecentlyChangedAll

decodeEntries :: Json -> Either JsonDecodeError (Array Entry)
decodeEntries json = decodeJson json

type Query
  = Array (Tuple String String)

makeQueryStr :: Query -> String
makeQueryStr query = joinWith "&" $ map (\x -> fst x <> "=" <> snd x) query

makeUrl :: String -> Query -> String
makeUrl base query = joinWith "?" $ filter (not null) [ base, makeQueryStr query ]

getRecentlyChanged :: Query -> Aff (Either String (Array Entry))
getRecentlyChanged query = do
  log url # liftEffect
  result1 <- AX.get ResponseFormat.json url
  pure $ case result1 of
    Left err -> Left ("GET failed: " <> (AX.printError err))
    Right response -> response.body # decodeEntries # lmap show -- # printEntriesOrError
  where
  url = makeUrl "https://api.ofdb.io/v0/entries/recently-changed" query

getRecentlyChangedAll :: Aff (Either String (Array Entry))
getRecentlyChangedAll = getRecentlyChanged []
