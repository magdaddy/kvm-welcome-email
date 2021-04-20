module WelcomeEmail.Server.Log where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import WelcomeEmail.Server.Winston as W
import WelcomeEmail.Shared.Entry (Entry)

data LogLevel
  = Error
  | Warn
  | Info
  | Http
  | Verbose
  | Debug
  | Silly

derive instance genericLogLevel :: Generic LogLevel _

instance showLogLevel :: Show LogLevel where
  show = genericShow


log :: String -> Effect Unit
log str = logL Info str

logL :: LogLevel -> String -> Effect Unit
-- logL level str = C.log $ (show level) <> ": " <> str
logL level str = W.log { level: winstonLevel level, message: str }


logSent :: Entry -> Boolean -> Effect Unit
logSent entry wasSent = W.log { level, message, wasSent, entry }
  where
  level = winstonLevel Info
  message
    | wasSent = "Email was sent to " <> entry.title <> "."
    | otherwise = "No email was sent to " <> entry.title <> " because it has no email address."


winstonLevel :: LogLevel -> String
winstonLevel = case _ of
  Error -> "error"
  Warn -> "warn"
  Info -> "info"
  Http -> "http"
  Verbose -> "verbose"
  Debug -> "debug"
  Silly -> "silly"
