module StateIO where

import Prelude

import Data.Argonaut (parseJson, stringifyWithIndent)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Effect (Effect)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (exists, readTextFile, writeTextFile)
import State (State, decodeJsonState, defaultState, encodeJsonState)

filename :: String
filename = "state.json"

loadState :: Effect (Either String State)
loadState = do
  fileExists <- exists filename
  if fileExists then do
    contents <- readTextFile UTF8 filename
    pure do
      json <- lmap show $ parseJson contents
      state <- decodeJsonState json
      pure state
  else
    pure $ Right defaultState

saveState :: State -> Effect Unit
saveState state = do
  let
    content = state # encodeJsonState # stringifyWithIndent 2
  writeTextFile UTF8 filename content
