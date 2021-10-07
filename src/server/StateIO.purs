module StateIO (loadState, saveState) where

import ThisPrelude

import Data.DateTime.Instant (Instant, instant, unInstant)
import Data.Time.Duration (Seconds(..), convertDuration)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (exists, readTextFile, writeTextFile)
import Record (modify)
import Simple.JSON (readJSON)
import Type.Prelude (Proxy(..))
import KvmMail.Shared.State (SavedState)
import KvmMail.Shared.Util (writeJSONPretty)


filename :: String
filename = "state.json"

type SavedStateJson = { latestInstant :: Maybe Number }

loadState :: Effect (Either String SavedState)
loadState = do
  fileExists <- exists filename
  if fileExists then do
    contents <- readTextFile UTF8 filename
    pure do
      statej <- lmap show $ readJSON contents
      let state = fromJson statej
      -- let stat = state { latestInstant = secToInst $ Just 1618740000.0 }
      pure state
  else
    pure $ Right { latestInstant: Nothing }

saveState :: SavedState -> Effect Unit
saveState state = writeTextFile UTF8 filename content
  where content = writeJSONPretty 2 $ toJson state

toJson :: SavedState -> SavedStateJson
toJson s = modify (Proxy :: _ "latestInstant") instToSec s

fromJson :: SavedStateJson -> SavedState
fromJson j = modify (Proxy :: _ "latestInstant") secToInst j

instToSec :: Maybe Instant -> Maybe Number
instToSec = map $ unwrap <<< (convertDuration <<< unInstant :: _ -> Seconds)

secToInst :: Maybe Number -> Maybe Instant
secToInst mbnsec = do
  nsec <- mbnsec
  instant $ convertDuration $ Seconds nsec
