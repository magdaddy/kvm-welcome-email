module WelcomeEmail.Server.Template where

import Prelude

import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), drop, indexOf, joinWith, splitAt)
import Effect (Effect)
import Effect.Exception (try)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile, writeTextFile)
import WelcomeEmail.Server.Data (AppError(..))
import WelcomeEmail.Server.Log (log)
import WelcomeEmail.Shared.Boundary (Email)
import WelcomeEmail.Shared.Template (EmailTemplate(..))


templateFn = "template.txt" :: String

loadTemplate :: Effect EmailTemplate
loadTemplate = do
  let filename = templateFn
  contents <- try $ readTextFile UTF8 filename
  let result = do
        cont <- lmap JsError contents
        parseEmail cont
  case result of
    Left err -> do
      log $ show err
      pure $ EmailTemplate { subject: "", body: "" }
    Right email -> pure $ EmailTemplate email

saveTemplate :: EmailTemplate -> Effect Unit
saveTemplate (EmailTemplate templ) = do
  let filename = templateFn
  let content = joinWith "\n" [templ.subject, templ.body]
  writeTextFile UTF8 filename content

parseEmail :: String -> Either AppError Email
parseEmail str = case indexOf (Pattern "\n") str of
  Nothing -> Left $ OtherError "Email parsing failed"
  Just i -> let { before, after } = splitAt i str in Right { subject: before, body: drop 1 after }
