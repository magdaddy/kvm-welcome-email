module Main where

import Prelude

import Data.Array (filter)
import Data.DateTime.Instant (instant)
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..), maybe)
import Data.Time.Duration (Milliseconds(..), Seconds(..), convertDuration)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, delay, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Now (now)
import Effect.Timer (setInterval)
import Log (appendLog)
import State (State)
import StateIO (loadState, saveState)
import WelcomeEmail.Server.Express (runServer)
import WelcomeEmail.Server.OfdbApi (getRecentlyChanged)
import WelcomeEmail.Shared.Entry (Entry, formatInstant, formatInstantUnix)
import WelcomeEmail.Shared.Util (unwrapOrThrow)




printEntryLine :: Entry -> Effect Unit
printEntryLine entry = log $ created <> " v" <> version <> " " <> entry.title
  where
  created = case ms # instant of
    Nothing -> "---"
    Just instant -> instant # formatInstant # show
  ms = Milliseconds (entry.created * 1000.0)
  version = entry.version # show

printEntries :: Array Entry -> Effect Unit
printEntries entries = traverse_ printEntryLine entries

sendEmails :: Array Entry -> Effect Unit
sendEmails entries = traverse_ sendEmail entries

sendEmail :: Entry -> Effect Unit
sendEmail entry = do
  -- send email
  appendLog entry

theloop :: State -> Aff Unit
theloop state = do
  let mbSince = state.latestInstant
  until <- now # liftEffect
  let s = maybe "" formatInstantUnix mbSince
  let u = formatInstantUnix until
  result <- getRecentlyChanged [Tuple "since" s, Tuple "until" u, Tuple "limit" (show 9)]
  case result of
    Left err -> do
      liftEffect $ log err
      delay $ convertDuration $ Seconds 5.0
      theloop state
    Right entries -> do
      let newEntries = filter (\e -> e.version == 0) entries
      liftEffect $ printEntries entries
      liftEffect $ sendEmails newEntries
      let newState = state { latestInstant = Just until }
      liftEffect $ saveState newState
      delay $ convertDuration $ Seconds 25.0
      theloop newState


main :: Effect Unit
main = do
  state <- loadState >>= unwrapOrThrow
  launchAff_ do
    -- liftEffect $ void $ setInterval 1000 (log ".")
    theloop state
  runServer
