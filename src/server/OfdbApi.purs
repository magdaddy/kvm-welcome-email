module KvmMail.Server.OfdbApi where

import ThisPrelude

import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import Data.Array as A
import Data.String as S
import Data.Tuple (Tuple, fst, snd)
import Simple.JSON (readJSON)
import KvmMail.Server.Log (LogLevel(..), logL)
import KvmMail.Shared.Entry (Entry, fromBEntry)


type Query
  = Array (Tuple String String)

makeQueryStr :: Query -> String
makeQueryStr query = S.joinWith "&" $ map (\x -> fst x <> "=" <> snd x) query

makeUrl :: String -> Query -> String
makeUrl base query = S.joinWith "?" $ A.filter (not S.null) [ base, makeQueryStr query ]

getRecentlyChanged :: Query -> Aff (Either String (Array Entry))
getRecentlyChanged query = do
  logL Verbose url # liftEffect
  result1 <- AX.get ResponseFormat.string url
  case result1 of
    Left err -> pure $ Left ("GET failed: " <> (AX.printError err))
    Right response -> do
      -- liftEffect $ logL Verbose $ show response.body
      pure $ lmap show $ rmap (map fromBEntry) $ readJSON $ response.body
  where
  url = makeUrl "https://api.ofdb.io/v0/entries/recently-changed" query

getRecentlyChangedAll :: Aff (Either String (Array Entry))
getRecentlyChangedAll = getRecentlyChanged []
