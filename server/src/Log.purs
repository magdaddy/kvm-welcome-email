module Log where

import Prelude

import Data.Array (mapMaybe)
import Data.Array.NonEmpty.Internal (NonEmptyArray(..))
import Data.DateTime.Instant (Instant)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split)
import Data.String.Regex (Regex, match)
import Data.String.Regex.Flags as Flags
import Data.String.Regex.Unsafe (unsafeRegex)
import Effect (Effect)
import Effect.Now (now)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (appendTextFile, readTextFile)
import WelcomeEmail.Shared.Entry (Entry, formatInstant)

filename :: String
filename = "sent.log"

logLine :: Instant -> Entry -> String
logLine ts entry = tsStr <> " | " <> entry.id <> " | v" <> version <> " | " <> entry.title <> "\n"
  where
  tsStr = ts # formatInstant
  version = entry.version # show

appendLog :: Entry -> Effect Unit
appendLog entry = do
  ts <- now
  appendTextFile UTF8 filename (logLine ts entry)
  pure unit

r1 = unsafeRegex """\|\s([0-9a-f]{32})\s\|""" Flags.global :: Regex
l1 = "2021-04-04 15:15:51 | 2c8e486e3ce4d74bb1b64247ed5351e | v0 | Gemeindehaus St. Lorenz" :: String

readLogIds :: Effect (Array String)
readLogIds = do
  content <- readTextFile UTF8 filename
  pure $ extractIdsFromLog content

-- simply ignore non-matching lines
extractIdsFromLog :: String -> Array String
extractIdsFromLog content = mapMaybe extractId lines
  where
  lines = split (Pattern "\n") content

  idRgx :: Regex
  idRgx = unsafeRegex """\|\s([0-9a-f]{32})\s\|""" Flags.noFlags

  extractId :: String -> Maybe String
  extractId line = case match idRgx line of
    Just (NonEmptyArray [_, (Just id)]) -> Just id
    _ -> Nothing
