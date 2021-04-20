module StateIO (loadState, saveState) where

import Prelude

import Data.Argonaut (parseJson, stringifyWithIndent)
import Data.Bifunctor (lmap)
import Data.DateTime.Instant (Instant, instant, unInstant)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Time.Duration (Seconds(..), convertDuration)
import Effect (Effect)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (exists, readTextFile, writeTextFile)
import Record (modify)
import Simple.JSON (readJSON)
import State (State, defaultState, encodeJsonState)
import Type.Prelude (Proxy(..))
import WelcomeEmail.Shared.Util (writeJSONPretty)


filename :: String
filename = "state.json"

type StateJson
  = { latestInstant :: Maybe Number
    , isRunning :: Boolean
    }

loadState :: Effect (Either String State)
loadState = do
  fileExists <- exists filename
  if fileExists then do
    contents <- readTextFile UTF8 filename
    pure do
      statej <- lmap show $ readJSON contents
      let state = fromJson statej
      -- json <- lmap show $ parseJson contents
      -- state <- decodeJsonState json
      -- 1618756036
      -- 1618750000
      let stat = state { latestInstant = secToInst $ Just 1618740000.0, isRunning = false }
      pure stat
  else
    pure $ Right defaultState

saveState :: State -> Effect Unit
saveState state = writeTextFile UTF8 filename content
  where content = writeJSONPretty 2 $ toJson state

toJson :: State -> StateJson
toJson s = modify (Proxy :: _ "latestInstant") instToSec s

fromJson :: StateJson -> State
fromJson j = modify (Proxy :: _ "latestInstant") secToInst j

instToSec :: Maybe Instant -> Maybe Number
instToSec = map $ unwrap <<< (convertDuration <<< unInstant :: _ -> Seconds)

secToInst :: Maybe Number -> Maybe Instant
secToInst mbnsec = do
  nsec <- mbnsec
  instant $ convertDuration $ Seconds nsec
