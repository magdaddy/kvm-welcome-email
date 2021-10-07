module KvmMail.App.Api.Backend where

import ThisPrelude

import Affjax (Request, Response, URL, defaultRequest, request)
import Affjax as AX
import Affjax.RequestBody (RequestBody, json)
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat as ResponseFormat
import Affjax.StatusCode (StatusCode(..))
import Control.Monad.Except (catchError, lift, throwError)
import Data.Argonaut (encodeJson)
import Data.Array as A
import Data.HTTP.Method (Method(..))
import MagLibs.DateFns (parseISO)
import Simple.JSON (class ReadForeign, readJSON)
import KvmMail.App.Api.Web (getTokenFromLocalStorage)
import KvmMail.App.Data (AppError(..))
import KvmMail.Shared.Boundary as B
import KvmMail.Shared.Entry (fromBEntry)
import KvmMail.Shared.Template (EmailTemplate)


login :: B.LoginData -> ExceptT AppError Aff String
login loginData = do
  let url = "/admin/login"
  let reqBody = json $ encodeJson $ loginData
  response <- (lift $ AX.post ResponseFormat.string url $ Just reqBody) >>= except # withExceptT HttpError
  case readJSON response.body of
    Left err -> case readJSON response.body of -- no token - could be error response
      Left _ -> throwError $ JsonError err
      Right ({ error } :: B.ErrorResponse) -> throwError $ ServerError error
    Right (tokenResponse :: B.TokenResponse) -> pure tokenResponse.token


toggleRunning :: ExceptT AppError Aff { isRunning :: Boolean }
toggleRunning = do
  let url = "/admin/togglerunning"
  liftEffect $ log url
  response <- reqAuth POST url Nothing
  sstate <- parseJsonResponse response
  pure sstate

serverState :: ExceptT AppError Aff { isRunning :: Boolean }
serverState = do
  let url = "/admin/serverstate"
  liftEffect $ log url
  response <- reqAuth GET url Nothing
  sstate <- parseJsonResponse response
  pure sstate


getTemplate :: ExceptT AppError Aff EmailTemplate
getTemplate = do
  let url = "/admin/template"
  liftEffect $ log url
  response <- reqAuth GET url Nothing
  template <- parseJsonResponse response
  pure template

saveTemplate :: EmailTemplate -> ExceptT AppError Aff Unit
saveTemplate template = do
  let url = "/admin/template"
  liftEffect $ log url
  let reqBody = json $ encodeJson template
  _ <- reqAuth POST url (Just reqBody)
  pure unit


getSettings :: ExceptT AppError Aff B.Settings
getSettings = do
  let url = "/admin/settings"
  liftEffect $ log url
  response <- reqAuth GET url Nothing
  settings <- parseJsonResponse response
  pure $ B.fromBSettings settings

saveSettings :: B.Settings -> ExceptT AppError Aff Unit
saveSettings settings = do
  let url = "/admin/settings"
  liftEffect $ log url
  let reqBody = json $ encodeJson $ B.toBSettings settings
  _ <- reqAuth POST url (Just reqBody)
  pure unit


sendTestMail :: B.TestMailPayload -> ExceptT AppError Aff B.TestMailResponse
sendTestMail pl = do
  let url = "/admin/sendtestmail"
  liftEffect $ log url
  let reqBody = json $ encodeJson pl
  response <- reqAuth POST url (Just reqBody)
  tmr <- parseJsonResponse response
  pure tmr
  -- result <- AX.post ResponseFormat.string url (Just reqBody)
  -- case result of
  --   Left err -> pure $ Left $ HttpError err
  --   Right response -> pure $ lmap JsonError $ readJSON response.body


getLastLogs :: ExceptT AppError Aff (Array B.LastLogEntry)
getLastLogs = do
  let url = "/admin/lastlogs"
  liftEffect $ log url
  response <- reqAuth GET url Nothing
  logLines :: Array B.LogLine <- parseJsonResponse response `catchError` \e -> do
    case e of
      JsonError _ -> do
        oldLogLines :: Array B.OldLogLine <- parseJsonResponse response
        pure $ map B.fromOldLogLine oldLogLines
      _ -> throwError e

  pure $ A.reverse $ A.mapMaybe toLogEntry logLines

toLogEntry :: B.LogLine -> Maybe B.LastLogEntry
toLogEntry { timestamp, level, message, wasSent, entry } = result
  where
  result
    | level == "error" = Just { timestamp: tsDate, type: B.Error message }
    | level == "warn" = Just { timestamp: tsDate, type: B.Warn message }
    | level == "info" && wasSent == Just true = case entry of
        Nothing -> Nothing
        Just e -> Just { timestamp: tsDate, type: B.EmailSent $ fromBEntry e }
    | otherwise = Nothing
  tsDate = parseISO timestamp

getRecentlyChanged :: forall m. MonadAff m => ExceptT AppError m (Array B.EntryChange)
getRecentlyChanged = do
  let url = "/admin/recently-changed"
  liftEffect $ log url
  response <- reqAuth GET url Nothing
  recentlyChanged :: B.EntryChangeA <- parseJsonResponse response >>= except <<< (note $ OtherError "deserialization failed") <<< B.deSer
  pure $ unwrap recentlyChanged
  -- pure $ reverse $ mapMaybe toLogEntry logLines

parseJsonResponse :: forall a m. MonadAff m => ReadForeign a => Response String -> ExceptT AppError m a
parseJsonResponse response = except $ lmap JsonError $ readJSON response.body

reqAuth :: forall m. MonadAff m => Method -> URL -> Maybe RequestBody -> ExceptT AppError m (Response String)
reqAuth method url reqBody = do
  token <- getToken
  let r = (authRequest token) { method = Left method, url = url, content = reqBody }
  response <- (liftAff $ request $ r) >>= except # withExceptT HttpError
  when (response.status == StatusCode 401) do
    { error: msg } :: B.ErrorResponse <- parseJsonResponse response
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

getToken :: forall m. MonadAff m => ExceptT AppError m String
getToken = liftEffect getTokenFromLocalStorage >>= case _ of
  Nothing -> throwError $ Unauthorized "No token"
  Just token -> pure token

