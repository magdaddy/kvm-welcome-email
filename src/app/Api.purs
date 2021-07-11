module WelcomeEmail.App.Api where

import Prelude

import Affjax as AX
import Affjax.RequestBody (json)
import Affjax.ResponseFormat as ResponseFormat
import Affjax.StatusCode (StatusCode(..))
import Data.Argonaut (encodeJson)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Simple.JSON (readJSON)
import WelcomeEmail.App.Data (AppError(..))
import WelcomeEmail.Shared.Template (EmailTemplate)


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


getTemplate :: Aff (Either AppError EmailTemplate)
getTemplate = do
  liftEffect $ log url
  result <- AX.get ResponseFormat.string url
  case result of
    Left err -> pure $ Left $ HttpError err
    Right response -> case response.status of
      -- StatusCode 200 -> pure $ Right $ split (Pattern "\n") response.body
      StatusCode 200 -> pure $ lmap JsonError $ readJSON response.body
      _ -> pure $ Left $ OtherError "server responded not-ok"
  where
  url = "http://localhost:4000/template"

saveTemplate :: EmailTemplate -> Aff (Either AppError Unit)
saveTemplate template = do
  liftEffect $ log url
  -- let reqBody = string (joinWith "\n" template)
  -- let reqBody = string (template.subject <> "\n" <> template.body)
  let reqBody = json $ encodeJson template
  _ <- AX.post ResponseFormat.string url (Just reqBody)
  pure $ Right unit
  where
  url = "http://localhost:4000/template"
