module KvmMail.Server.LastLogs where

import ThisPrelude

import Control.Monad.Except (lift)
import Data.Array (dropEnd, takeEnd)
import Data.String (Pattern(..), joinWith, split)
import Effect.Aff (try)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import KvmMail.Server.Data (AppError(..))


loadLastLogs :: Int -> ExceptT AppError Aff String
loadLastLogs count = do
  resContent <- lift $ try $ readTextFile UTF8 "logfile.log"
  content <- except $ lmap JsError resContent
  let lines = dropEnd 1 $ takeEnd count $ split (Pattern "\n") content
  let json = "[" <> joinWith "," lines <> "]"
  pure json
  -- let resLogLines = map parseLogLine lines
  -- logLines :: Array LogLine <- case find isLeft resLogLines of
  --   Just (Left err) -> except (Left err)
  --   _ -> pure $ mapMaybe hush resLogLines
  -- pure $ mapMaybe toLogEntry logLines

-- toLogEntry :: LogLine -> Maybe LastLogEntry
-- toLogEntry { timeStamp, level, message }
--   | level == "error" = Just { timeStamp, type: Error message }
--   | level == "warn" = Just { timeStamp, type: Warn message }
--   | otherwise = Nothing

-- parseLogLine :: String -> Either AppError LogLine
-- parseLogLine s = lmap JsonError $ readJSON s
