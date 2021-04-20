module WelcomeEmail.Shared.Entry where

import Prelude

import Data.DateTime (DateTime)
import Data.DateTime.Instant (Instant, instant, toDateTime, unInstant)
import Data.Either (Either(..))
import Data.Formatter.DateTime as Format.DateTime
import Data.Formatter.Number as Format.Number
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Time.Duration (Seconds(..), convertDuration)
import Partial.Unsafe (unsafePartial)

type Entry
  = { id :: String
    , title :: String
    , created :: Number
    , version :: Int
    , contact_name :: Maybe String
    , email :: Maybe String
    }

formatDateTime :: String -> DateTime -> String
formatDateTime formatStr dt = Format.DateTime.format formatter dt
  where
  formatter = unsafePartial case Format.DateTime.parseFormatString formatStr of Right x -> x

formatDtLog :: DateTime -> String
formatDtLog = formatDateTime "YYYY-MM-DD HH:mm:ss"

formatDtEmail :: DateTime -> String
formatDtEmail dt  = (formatDateTime "DD.MM." dt) <> " um " <> (formatDateTime "HH:mm" dt) <> " Uhr"

formatInstant :: Instant -> String
formatInstant instant = instant # toDateTime # formatDtLog

formatInstantUnix :: Instant -> String
formatInstantUnix instant = Format.Number.format formatter (instant # toSeconds # unwrap)
  where
  formatter = unsafePartial case Format.Number.parseFormatString "0" of Right x -> x

toSeconds :: Instant -> Seconds
toSeconds i = i # unInstant # convertDuration

formatCreatedEmail :: Number -> String
formatCreatedEmail created = case instant $ convertDuration $ Seconds created of
  Nothing -> "---"
  Just inst -> formatDtEmail $ toDateTime inst
