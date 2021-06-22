module WelcomeEmail.Shared.State where

import Prelude

import Data.DateTime.Instant (Instant, instant, unInstant)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Seconds(..), convertDuration)
import Effect.Aff (Fiber)
import Math (round)

-- type State
--   = { latestInstant :: Maybe Instant
--     , isRunning :: Boolean
--     -- , nodeMailer :: TransportConfig
--     -- , defaultEntry :: Entry
--     }

type State =
  { running :: Maybe (Fiber Unit)
  , saved :: SavedState
  }

type SavedState = { latestInstant :: Maybe Instant }


defaultState :: State
defaultState =
  { running: Nothing
  , saved: { latestInstant: Nothing }
  }
-- defaultState :: State
-- defaultState =
--   { latestInstant: Nothing
--   , isRunning: false
--   }

fromUnixTsNumber :: Number -> Maybe Instant
fromUnixTsNumber n = n # Seconds # convertDuration # instant

toUnixTsNumber :: Instant -> Number
toUnixTsNumber i = round n
  where
  Seconds n = i # unInstant # convertDuration


-- encodeJsonState :: State -> Json
-- encodeJsonState state =
--   "latestInstant" :=? (map toUnixTsNumber state.latestInstant)
--     -- ~>? "nodeMailer" := state.nodeMailer
--     -- ~> "defaultEntry" := state.defaultEntry
--     ~>? jsonEmptyObject

