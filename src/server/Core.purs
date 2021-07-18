module WelcomeEmail.Server.Core where

import Prelude

import Data.Array (filter)
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Hours(..), convertDuration)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff, delay)
import Effect.Class (liftEffect)
import Effect.Now (now)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Nmailer (mkMessage, send)
import NodeMailer (TransportConfig)
import StateIO (saveState)
import WelcomeEmail.Server.Log (LogLevel(..), logL, logSent)
import WelcomeEmail.Server.OfdbApi (getRecentlyChanged)
import WelcomeEmail.Server.Settings (loadSettings)
import WelcomeEmail.Server.Template (loadTemplate)
import WelcomeEmail.Server.Util (isInDach)
import WelcomeEmail.Shared.Entry (Entry, formatInstantUnix)
import WelcomeEmail.Shared.State (State)
import WelcomeEmail.Shared.Template (EmailTemplate, expand)


sendEmails :: Array Entry -> Aff Unit
sendEmails entries = do
  settings <- liftEffect loadSettings
  templ <- liftEffect loadTemplate
  traverse_ (sendEmail settings.nodeMailer settings.senderAddress templ) entries

sendEmail :: TransportConfig () -> String -> EmailTemplate -> Entry -> Aff Unit
sendEmail conf senderAddr templ entry = do
  let email = expand entry templ
  case entry.email of
    Nothing -> liftEffect $ logSent entry false
    Just toAddr -> do
      let msg = mkMessage senderAddr toAddr email
      result <- send conf msg
      case result of
        Left err -> liftEffect $ logL Error $ show err
        Right _ -> liftEffect $ logSent entry true

theloop :: Ref State -> Aff Unit
theloop stateRef = do
  state <- liftEffect $ Ref.read stateRef
  -- let mbSince = state.latestInstant
  since <- case state.saved.latestInstant of
    Nothing -> liftEffect now
    Just inst -> pure inst
  until <- now # liftEffect
  -- let s = maybe "" formatInstantUnix mbSince
  let s = formatInstantUnix since
  let u = formatInstantUnix until
  -- result <- getRecentlyChanged [Tuple "since" s, Tuple "until" u, Tuple "limit" (show 9)]
  result <- getRecentlyChanged [Tuple "since" s, Tuple "until" u]
  case result of
    Left err -> do
      liftEffect $ logL Error err
      delay $ convertDuration $ Hours 1.0
      theloop stateRef
    Right entries -> do
      let newEntries = filter (\e -> e.version == 0 && isInDach e) entries
      -- liftEffect $ printEntries entries
      sendEmails newEntries
      newState <- liftEffect $ Ref.modify _ { saved { latestInstant = Just until } } stateRef
      liftEffect $ saveState newState.saved
      delay $ convertDuration $ Hours 1.0
      theloop stateRef
