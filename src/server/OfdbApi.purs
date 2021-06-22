module WelcomeEmail.Server.OfdbApi where

import Prelude

import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import Data.Array (filter)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.String (joinWith, null)
import Data.Tuple (Tuple, fst, snd)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Simple.JSON (readJSON)
import WelcomeEmail.Server.Log (LogLevel(..), logL)
import WelcomeEmail.Shared.Entry (Entry)


type Query
  = Array (Tuple String String)

makeQueryStr :: Query -> String
makeQueryStr query = joinWith "&" $ map (\x -> fst x <> "=" <> snd x) query

makeUrl :: String -> Query -> String
makeUrl base query = joinWith "?" $ filter (not null) [ base, makeQueryStr query ]

getRecentlyChanged :: Query -> Aff (Either String (Array Entry))
getRecentlyChanged query = do
  logL Verbose url # liftEffect
  result1 <- AX.get ResponseFormat.string url
  case result1 of
    Left err -> pure $ Left ("GET failed: " <> (AX.printError err))
    Right response -> do
      -- liftEffect $ logL Verbose $ show response.body
      pure $ lmap show $ readJSON $ response.body
  where
  url = makeUrl "https://api.ofdb.io/v0/entries/recently-changed" query

getRecentlyChangedAll :: Aff (Either String (Array Entry))
getRecentlyChangedAll = getRecentlyChanged []
