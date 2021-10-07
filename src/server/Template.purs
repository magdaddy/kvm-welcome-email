module KvmMail.Server.Template where

import ThisPrelude hiding (log)

import Data.String as S
import Effect.Exception (try)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile, writeTextFile)
import KvmMail.Server.Data (AppError(..))
import KvmMail.Server.Log (log)
import KvmMail.Shared.Boundary (Email)
import KvmMail.Shared.Template (EmailTemplate(..))


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
  let content = S.joinWith "\n" [templ.subject, templ.body]
  writeTextFile UTF8 filename content

parseEmail :: String -> Either AppError Email
parseEmail str = case S.indexOf (S.Pattern "\n") str of
  Nothing -> Left $ OtherError "Email parsing failed"
  Just i -> let { before, after } = S.splitAt i str in Right { subject: before, body: S.drop 1 after }
