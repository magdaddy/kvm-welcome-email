module WelcomeEmail.App.AppM where

import Prelude

import Affjax as AX
import Affjax.RequestBody (string)
import Affjax.ResponseFormat as ResponseFormat
import Affjax.StatusCode (StatusCode(..))
import Control.Monad.Reader.Trans (class MonadAsk, ReaderT, asks, runReaderT)
import Control.Parallel (class Parallel, parallel, sequential)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff, ParAff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log)
import Simple.JSON (readJSON, writeJSON)
import Type.Equality (class TypeEquals, from)
import WelcomeEmail.App.Caps (class ManageSettings, class ManageTemplate, class SendTestMail)
import WelcomeEmail.App.Data (AppError(..))
import WelcomeEmail.App.Env (Env)

newtype AppM a = AppM (ReaderT Env Aff a)

runAppM :: Env -> AppM ~> Aff
runAppM env (AppM m) = runReaderT m env

derive newtype instance functorAppM :: Functor AppM
derive newtype instance applyAppM :: Apply AppM
derive newtype instance applicativeAppM :: Applicative AppM
derive newtype instance bindAppM :: Bind AppM
derive newtype instance monadAppM :: Monad AppM
derive newtype instance monadEffectAppM :: MonadEffect AppM
derive newtype instance monadAffAppM :: MonadAff AppM

instance monadAskAppM :: TypeEquals e Env => MonadAsk e AppM where
  ask = AppM $ asks from

newtype ParAppM a
  = ParAppM (ReaderT Env ParAff a)

derive newtype instance functorParAppM :: Functor ParAppM
derive newtype instance applyParAppM :: Apply ParAppM
derive newtype instance applicativeParAppM :: Applicative ParAppM

instance parallelAppM :: Parallel ParAppM AppM where
  parallel (AppM readerT) = ParAppM (parallel readerT)
  sequential (ParAppM readerT) = AppM (sequential readerT)


instance manageTemplateAppM :: ManageTemplate AppM where
  getTemplate = do
    log url # liftEffect
    result <- liftAff $ AX.get ResponseFormat.string url
    case result of
      Left err -> pure $ Left $ HttpError err
      Right response -> case response.status of
        -- StatusCode 200 -> pure $ Right $ split (Pattern "\n") response.body
        StatusCode 200 -> pure $ lmap JsonError $ readJSON response.body
        _ -> pure $ Left $ OtherError "server responded not-ok"
    where
    url = "http://localhost:4000/template"
  saveTemplate template = do
    log url # liftEffect
    -- let reqBody = string (joinWith "\n" template)
    -- let reqBody = string (template.subject <> "\n" <> template.body)
    let reqBody = string $ writeJSON template
    result <- liftAff $ AX.post ResponseFormat.string url (Just reqBody)
    pure $ Right unit
    where
    url = "http://localhost:4000/template"


instance manageSettingsAppM :: ManageSettings AppM where
  getSettings = do
    log url # liftEffect
    result <- liftAff $ AX.get ResponseFormat.string url
    case result of
      Left err -> pure $ Left $ HttpError err
      Right response -> case response.status of
        StatusCode 200 -> pure $ lmap JsonError $ readJSON response.body
        _ -> pure $ Left $ OtherError "server responded not-ok"
    where
    url = "http://localhost:4000/settings"
  saveSettings settings = do
    log url # liftEffect
    let reqBody = string $ writeJSON settings
    result <- liftAff $ AX.post ResponseFormat.json url (Just reqBody)
    pure $ Right unit
    where
    url = "http://localhost:4000/settings"


instance sendTestMailAppM :: SendTestMail AppM where
  sendTestMail pl = do
    log url # liftEffect
    let reqBody = string $ writeJSON pl
    result <- liftAff $ AX.post ResponseFormat.string url (Just reqBody)
    case result of
      Left err -> pure $ Left $ HttpError err
      Right response -> pure $ lmap JsonError $ readJSON response.body
    where
    url = "http://localhost:4000/sendtestmail"
