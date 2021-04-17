module State where

import Prelude

import Data.Argonaut (Json, JsonDecodeError(..), decodeJson, jsonEmptyObject, (.:?), (:=), (:=?), (~>), (~>?))
import Data.Bifunctor (lmap)
import Data.DateTime.Instant (Instant, instant, unInstant)
import Data.Either (Either, note)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Seconds(..), convertDuration)
import Math (round)
import WelcomeEmail.Shared.Entry (Entry)

type AuthConfig =
  { user :: String
  , pass :: String
  }

type TransportConfig =
  { host :: String
  , port :: Int
  , secure :: Boolean
  , auth :: AuthConfig
  }

type State
  = { latestInstant :: Maybe Instant
    , nodeMailer :: TransportConfig
    , defaultEntry :: Entry
    }

defaultState :: State
defaultState =
  { latestInstant: Nothing
  , nodeMailer:
      { auth:
          { pass: ""
          , user: ""
          }
      , host: "in-v3.mailjet.com"
      , port: 587
      , secure: false
      }
  , defaultEntry:
      { id: "4c20979fe0754e74875afa4308d73ce7"
      , title: "Slowtec GmbH"
      , created: 1234.0
      , version: 33
      }
  }

fromUnixTsNumber :: Number -> Maybe Instant
fromUnixTsNumber n = n # Seconds # convertDuration # instant

toUnixTsNumber :: Instant -> Number
toUnixTsNumber i = round n
  where
  Seconds n = i # unInstant # convertDuration

decodeJsonState :: Json -> Either String State
decodeJsonState json =
  lmap show do
    obj <- decodeJson json
    latestInstantNum <- obj .:? "latestInstant"
    latestInstant <- note (TypeMismatch "latestInstant") (map fromUnixTsNumber latestInstantNum)
    mbNodeMailer <- obj .:? "nodeMailer"
    nodeMailer <- note (TypeMismatch "nodeMailer") mbNodeMailer
    mbDefaultEntry <- obj .:? "defaultEntry"
    defaultEntry <- note (TypeMismatch "defaultEntry") mbDefaultEntry
    pure { latestInstant, nodeMailer, defaultEntry }

encodeJsonState :: State -> Json
encodeJsonState state =
  "latestInstant" :=? (map toUnixTsNumber state.latestInstant)
    ~>? "nodeMailer" := state.nodeMailer
    ~> "defaultEntry" := state.defaultEntry
    ~> jsonEmptyObject

