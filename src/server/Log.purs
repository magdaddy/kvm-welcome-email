module KvmMail.Server.Log where

import ThisPrelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Simple.JSON (write)
import KvmMail.Server.Winston as W
import KvmMail.Shared.Entry (Entry, toBEntry)

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


log :: forall m. MonadEffect m => String -> m Unit
log str = logL Info str

logL :: forall m. MonadEffect m => LogLevel -> String -> m Unit
-- logL level str = C.log $ (show level) <> ": " <> str
logL level str = liftEffect $ W.log { level: winstonLevel level, message: str }


logSent :: forall m. MonadEffect m => Entry -> Boolean -> m Unit
logSent entry wasSent = liftEffect $ W.log { level, message, wasSent, entry: write $ toBEntry entry }
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
