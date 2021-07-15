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
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Simple.JSON (class ReadForeign, readJSON)
import WelcomeEmail.App.Api.Web (getTokenFromLocalStorage)
import WelcomeEmail.App.Data (AppError(..))
import WelcomeEmail.Shared.Boundary (ErrorResponse, LoginData, Settings, TestMailPayload, TestMailResponse, TokenResponse)
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


toggleRunning :: Aff (Either AppError { isRunning :: Boolean })
toggleRunning = do
  liftEffect $ log url
  result <- AX.post ResponseFormat.string url Nothing
  case result of
    Left err -> pure $ Left $ HttpError err
    Right response -> case response.status of
      StatusCode 200 -> pure $ lmap JsonError $ readJSON response.body
      _ -> pure $ Left $ OtherError "server responded not-ok"
  where
  url = "http://localhost:4000/togglerunning"

serverState :: Aff (Either AppError { isRunning :: Boolean })
serverState = do
  liftEffect $ log url
  result <- AX.get ResponseFormat.string url
  case result of
    Left err -> pure $ Left $ HttpError err
    Right response -> case response.status of
      StatusCode 200 -> pure $ lmap JsonError $ readJSON response.body
      _ -> pure $ Left $ OtherError "server responded not-ok"
  where
  url = "http://localhost:4000/serverstate"


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


sendTestMail :: TestMailPayload -> Aff (Either AppError TestMailResponse)
sendTestMail pl = do
  liftEffect $ log url
  let reqBody = json $ encodeJson pl
  result <- AX.post ResponseFormat.string url (Just reqBody)
  case result of
    Left err -> pure $ Left $ HttpError err
    Right response -> pure $ lmap JsonError $ readJSON response.body
  where
  url = "http://localhost:4000/sendtestmail"


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





-- getTemplate :: Aff (Either AppError EmailTemplate)
-- getTemplate = do
--   liftEffect $ log url
--   result <- AX.get ResponseFormat.string url
--   case result of
--     Left err -> pure $ Left $ HttpError err
--     Right response -> case response.status of
--       -- StatusCode 200 -> pure $ Right $ split (Pattern "\n") response.body
--       StatusCode 200 -> pure $ lmap JsonError $ readJSON response.body
--       _ -> pure $ Left $ OtherError "server responded not-ok"
--   where
--   url = "http://localhost:4000/template"

-- saveTemplate :: EmailTemplate -> Aff (Either AppError Unit)
-- saveTemplate template = do
--   liftEffect $ log url
--   -- let reqBody = string (joinWith "\n" template)
--   -- let reqBody = string (template.subject <> "\n" <> template.body)
--   let reqBody = json $ encodeJson template
--   _ <- AX.post ResponseFormat.string url (Just reqBody)
--   pure $ Right unit
--   where
--   url = "http://localhost:4000/template"

-- getSettings :: Aff (Either AppError Settings)
-- getSettings = do
--   liftEffect $ log url
--   result <- AX.get ResponseFormat.string url
--   case result of
--     Left err -> pure $ Left $ HttpError err
--     Right response -> case response.status of
--       StatusCode 200 -> pure $ lmap JsonError $ readJSON response.body
--       _ -> pure $ Left $ OtherError "server responded not-ok"
--   where
--   url = "http://localhost:4000/api/settings"

-- saveSettings :: Settings -> Aff (Either AppError Unit)
-- saveSettings settings = do
--   liftEffect $ log url
--   let reqBody = json $ encodeJson settings
--   _ <- AX.post ResponseFormat.json url (Just reqBody)
--   pure $ Right unit
--   where
--   url = "http://localhost:4000/settings"

