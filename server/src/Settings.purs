module WelcomeEmail.Server.Settings where

import Prelude

import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), drop, indexOf, splitAt)
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (try)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile, writeTextFile)
import Simple.JSON (readJSON, writeJSON)
import WelcomeEmail.Server.Data (AppError(..))
import WelcomeEmail.Shared.Boundary (Settings, defaultSettings)
import WelcomeEmail.Shared.Util (writeJSONPretty)


settingsFn = "settings.json" :: String

loadSettings :: Effect Settings
loadSettings = do
  let filename = settingsFn
  contents <- try $ readTextFile UTF8 filename
  let result = do
        cont <- lmap JsError contents
        lmap JsonError $ readJSON cont
  case result of
    Left err -> do
      log $ show err
      pure defaultSettings
    Right email -> pure email

saveSettings :: Settings -> Effect Unit
saveSettings settings = do
  let filename = settingsFn
  writeTextFile UTF8 filename $ writeJSONPretty 2 settings

