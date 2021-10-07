module KvmMail.Shared.State where

import ThisPrelude

import Data.DateTime.Instant (Instant, instant, unInstant)
import Data.Time.Duration (Seconds(..), convertDuration)
import Effect.Aff (Fiber)
import Math (round)


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

fromUnixTsNumber :: Number -> Maybe Instant
fromUnixTsNumber n = n # Seconds # convertDuration # instant

toUnixTsNumber :: Instant -> Number
toUnixTsNumber i = round n
  where
  Seconds n = i # unInstant # convertDuration
