module WelcomeEmail.App.Api.Backend where

import Prelude

import Affjax (Request, Response, URL, defaultRequest, request)
import Affjax as AX
import Affjax.RequestBody (RequestBody, json)
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat as ResponseFormat
import Affjax.StatusCode (StatusCode(..))
import Control.Monad.Except (ExceptT, catchError, except, lift, throwError, withExceptT)
import Data.Argonaut (encodeJson)
import Data.Array (mapMaybe, reverse)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import MagLibs.DateFns (parseISO)
import Simple.JSON (class ReadForeign, readJSON)
import WelcomeEmail.App.Api.Web (getTokenFromLocalStorage)
import WelcomeEmail.App.Data (AppError(..))
import WelcomeEmail.Shared.Boundary (ErrorResponse, LastLogEntry, LastLogType(..), LogLine, LoginData, Settings, TestMailPayload, TestMailResponse, TokenResponse)
import WelcomeEmail.Shared.Template (EmailTemplate)


login :: LoginData -> ExceptT AppError Aff String
login loginData = do
  let url = "/api/login"
  let reqBody = json $ encodeJson $ loginData
  response <- (lift $ AX.post ResponseFormat.string url $ Just reqBody) >>= except # withExceptT HttpError
  case readJSON response.body of
    Left err -> case readJSON response.body of -- no token - could be error response
      Left _ -> throwError $ JsonError err
      Right ({ error } :: ErrorResponse) -> throwError $ ServerError error
    Right (tokenResponse :: TokenResponse) -> pure tokenResponse.token


toggleRunning :: ExceptT AppError Aff { isRunning :: Boolean }
toggleRunning = do
  let url = "/api/togglerunning"
  liftEffect $ log url
  response <- reqAuth POST url Nothing
  sstate <- parseJsonResponse response
  pure sstate

serverState :: ExceptT AppError Aff { isRunning :: Boolean }
serverState = do
  let url = "/api/serverstate"
  liftEffect $ log url
  response <- reqAuth GET url Nothing
  sstate <- parseJsonResponse response
  pure sstate


getTemplate :: ExceptT AppError Aff EmailTemplate
getTemplate = do
  let url = "/api/template"
  liftEffect $ log url
  response <- reqAuth GET url Nothing
  template <- parseJsonResponse response
  pure template

saveTemplate :: EmailTemplate -> ExceptT AppError Aff Unit
saveTemplate template = do
  let url = "/api/template"
  liftEffect $ log url
  let reqBody = json $ encodeJson template
  _ <- reqAuth POST url (Just reqBody)
  pure unit


getSettings :: ExceptT AppError Aff Settings
getSettings = do
  let url = "/api/settings"
  liftEffect $ log url
  response <- reqAuth GET url Nothing
  settings <- parseJsonResponse response
  pure settings

saveSettings :: Settings -> ExceptT AppError Aff Unit
saveSettings settings = do
  let url = "/api/settings"
  liftEffect $ log url
  let reqBody = json $ encodeJson settings
  _ <- reqAuth POST url (Just reqBody)
  pure unit


sendTestMail :: TestMailPayload -> ExceptT AppError Aff TestMailResponse
sendTestMail pl = do
  let url = "/api/sendtestmail"
  liftEffect $ log url
  let reqBody = json $ encodeJson pl
  response <- reqAuth POST url (Just reqBody)
  tmr <- parseJsonResponse response
  pure tmr
  -- result <- AX.post ResponseFormat.string url (Just reqBody)
  -- case result of
  --   Left err -> pure $ Left $ HttpError err
  --   Right response -> pure $ lmap JsonError $ readJSON response.body


getLastLogs :: ExceptT AppError Aff (Array LastLogEntry)
getLastLogs = do
  let url = "/api/lastlogs"
  liftEffect $ log url
  response <- reqAuth GET url Nothing
  logLines :: Array LogLine <- parseJsonResponse response
  -- liftEffect $ log $ show logLines
  -- let resLogLines = map parseLogLine lines
  -- logLines :: Array LogLine <- case find isLeft resLogLines of
  --   Just (Left err) -> except (Left err)
  --   _ -> pure $ mapMaybe hush resLogLines
  pure $ reverse $ mapMaybe toLogEntry logLines

toLogEntry :: LogLine -> Maybe LastLogEntry
toLogEntry { timestamp, level, message, wasSent, entry } = result
  where
  result
    | level == "error" = Just { timestamp: tsDate, type: Error message }
    | level == "warn" = Just { timestamp: tsDate, type: Warn message }
    | level == "info" && wasSent == Just true = case entry of
        Nothing -> Nothing
        Just e -> Just { timestamp: tsDate, type: EmailSent e }
    | otherwise = Nothing
  tsDate = parseISO timestamp

-- parseLogLine :: String -> Either AppError LogLine
-- parseLogLine s = lmap JsonError $ readJSON s


parseJsonResponse :: forall a. ReadForeign a => Response String -> ExceptT AppError Aff a
parseJsonResponse response = except $ lmap JsonError $ readJSON response.body

reqAuth :: Method -> URL -> Maybe RequestBody -> ExceptT AppError Aff (Response String)
reqAuth method url reqBody = do
  token <- getToken
  let r = (authRequest token) { method = Left method, url = url, content = reqBody }
  response <- (lift $ request $ r) >>= except # withExceptT HttpError
  when (response.status == StatusCode 401) do
    { error: msg } :: ErrorResponse <- parseJsonResponse response
      `catchError` \_e -> pure { error: response.body }
    throwError $ Unauthorized msg
  when (response.status /= StatusCode 200) do
    throwError $ ServerError "server responded not-ok"
  pure response

authRequest :: String -> Request String
authRequest token = defaultRequest
  { responseFormat = ResponseFormat.string
  , headers = [RequestHeader "Authorization" $ "Bearer " <> token]
  }

getToken :: ExceptT AppError Aff String
getToken = liftEffect getTokenFromLocalStorage >>= case _ of
  Nothing -> throwError $ Unauthorized "No token"
  Just token -> pure token

