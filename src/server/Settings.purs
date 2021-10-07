module WelcomeEmail.Server.Settings where

import ThisPrelude hiding (log)

import Effect.Exception (try)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile, writeTextFile)
import Simple.JSON (readJSON)
import WelcomeEmail.Server.Data (AppError(..))
import WelcomeEmail.Server.Log (log)
import WelcomeEmail.Shared.Boundary (Settings, defaultSettings, fromOldBSettings, toBSettings)
import WelcomeEmail.Shared.Util (writeJSONPretty)


settingsFn = "settings.json" :: String

loadSettings :: Effect Settings
loadSettings = do
  let filename = settingsFn
  contents <- try $ readTextFile UTF8 filename
  let result = do
        cont <- lmap JsError contents
        lmap JsonError $ rmap fromOldBSettings $ readJSON cont
  case result of
    Left err -> do
      log $ show err
      pure defaultSettings
    Right email -> pure email

saveSettings :: Settings -> Effect Unit
saveSettings settings = do
  let filename = settingsFn
  writeTextFile UTF8 filename $ writeJSONPretty 2 $ toBSettings settings

