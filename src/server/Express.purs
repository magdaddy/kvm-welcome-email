module KvmMail.Server.Express where

import ThisPrelude hiding (log)

import Control.Monad.Except (lift, throwError)
import Data.Array as A
import Data.String as S
import Effect.Aff (error, forkAff, killFiber)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Nmailer (NMailer, sendTestMail)
import Node.Express.App (App, get, listenHostHttp, post, use, useExternal)
import Node.Express.Handler (Handler, HandlerM)
import Node.Express.Middleware.Static (static)
import Node.Express.Request (getBody', getQueryParam, getRequestHeader)
import Node.Express.Response (send, setStatus)
import Node.Express.Types (Middleware)
import Node.HTTP (Server) as Http
import Simple.JSON (class ReadForeign, read, writeJSON)
import KvmMail.Server.Core (theloop)
import KvmMail.Server.Data (AppError(..))
import KvmMail.Server.LastLogs (loadLastLogs)
import KvmMail.Server.Log (LogLevel(..), log, logL)
import KvmMail.Server.Services.CrudRepo (FileRepo)
import KvmMail.Server.Services.RecentlyChanged (defaultRecentlyChangedFiles, recentlyChanged)
import KvmMail.Server.Services.SingletonRepo (SingFileRepo, load, save)
import KvmMail.Server.Subscription.Api (ConfirmError(..), SubscribePayload, UnsubscribeError(..))
import KvmMail.Server.Subscription.Api as SubscriptionApi
import KvmMail.Server.Template (loadTemplate, saveTemplate)
import KvmMail.Server.Util (getUsers, jwtSign, jwtVerify, tokenSecret)
import KvmMail.Server.Winston (morgan)
import KvmMail.Shared.Boundary (BSettings, EntryChangeA(..), LoginData, TestMailPayload, Settings, fromBSettings, ser, toBSettings)
import KvmMail.Shared.State (State)
import KvmMail.Shared.Template (EmailTemplate)


type Env =
  { subscription :: { repo :: FileRepo, mailer :: NMailer, apiBaseUrl :: String }
  , settingsRepo :: SingFileRepo Settings
  }

foreign import cors :: Middleware
foreign import text :: Middleware
foreign import json :: Middleware

server :: Ref State -> Env -> App
server stateRef env = do
  useExternal morgan
  useExternal cors
  useExternal text
  useExternal json

  -- admin --

  post "/admin/login" do
    withErrorHandler defErrHandler do
      loginData :: LoginData <- parseJsonReqBody
      result <- liftEffect $ getUsers
      users <- except $ lmap (\err -> OtherError $ "Something's wrong with the users... this should never happen." <> err) result
      case A.find (\u -> u.name == loginData.username && u.pwd == loginData.password) users of
        Nothing -> lift $ send { error: "Login failed, wrong credentials." }
        Just _ -> do
          let jwt = jwtSign {} tokenSecret { subject: loginData.username, expiresIn: "1w" }
          lift $ send { token: jwt }

  post "/admin/sendtestmail" do
    withErrorHandler defErrHandler do
      ensureAuthorized
      pl :: TestMailPayload <- parseJsonReqBody
      log $ show pl
      result <- liftAff $ sendTestMail pl.emailAddr
      except result
      lift $ send unit

  get "/admin/settings" do
    withErrorHandler defErrHandler do
      ensureAuthorized
      -- settings <- liftEffect loadSettings
      settings <- load env.settingsRepo
      lift $ send $ writeJSON $ toBSettings settings

  post "/admin/settings" do
    withErrorHandler defErrHandler do
      ensureAuthorized
      settings :: BSettings <- parseJsonReqBody
      -- liftEffect $ saveSettings $ fromBSettings settings
      save (fromBSettings settings) env.settingsRepo
      lift $ send unit

  get "/admin/template" do
    withErrorHandler defErrHandler do
      ensureAuthorized
      email <- liftEffect loadTemplate
      lift $ send $ writeJSON email

  post "/admin/template" do
    withErrorHandler defErrHandler do
      ensureAuthorized
      templ :: EmailTemplate <- parseJsonReqBody
      liftEffect $ saveTemplate templ
      lift $ send unit

  get "/admin/serverstate" do
    withErrorHandler defErrHandler do
      ensureAuthorized
      state <- liftEffect $ Ref.read stateRef
      case state.running of
        Nothing -> lift $ send $ writeJSON { isRunning: false }
        Just _fiber -> lift $ send $ writeJSON { isRunning: true }

  post "/admin/togglerunning" do
    withErrorHandler defErrHandler do
      ensureAuthorized
      state <- liftEffect $ Ref.read stateRef
      case state.running of
        Nothing -> do
          fiber <- liftAff $ forkAff $ theloop stateRef
          liftEffect $ Ref.modify_ _ { running = Just fiber } stateRef
          logL Warn "Sending-service started"
          lift $ send $ writeJSON { isRunning: true }
        Just fiber -> do
          liftAff $ killFiber (error "stop requested") fiber
          liftEffect $ Ref.modify_ _ { running = Nothing } stateRef
          logL Warn "Sending-service stopped"
          lift $ send $ writeJSON { isRunning: false }

  get "/admin/lastlogs" do
    withErrorHandler defErrHandler do
      ensureAuthorized
      result <- liftAff $ runExceptT $ loadLastLogs 300
      lastLogs <- except result
      lift $ send $ lastLogs

  get "/admin/recently-changed" do
    withErrorHandler defErrHandler do
      ensureAuthorized
      ecs <- recentlyChanged defaultRecentlyChangedFiles # withExceptT (OtherError <<< show)
      lift $ send $ writeJSON $ ser $ EntryChangeA ecs

  -- subscribe --

  post "/api/subscribe" do
    withErrorHandler apiErrHandler do
      pl :: SubscribePayload <- parseJsonReqBody
      -- SubscriptionApi.subscribe pl env.subscription.apiBaseUrl >>= except
      _confToken <- flip runReaderT env (SubscriptionApi.subscribeFlow pl) >>= except
      lift $ send unit

  get "/api/confirm-subscription" do
    let
      handler e = send $ message e
      message = case _ of
        CENoToken -> "The confirmation token is missing"
        CESubscriptionDoesNotExist -> "This subscription is not in our database. Maybe you waited too long to confirm?"
        CEOtherError msg -> msg
    withErrorHandler handler do
      mbToken <- lift $ getQueryParam "token"
      token <- except $ note CENoToken mbToken
      SubscriptionApi.confirm token # flip runReaderT env >>= except
      lift $ send $ "Email confirmed, subscription active"

  get "/api/unsubscribe" do
    let
      handler e = send $ message e
      message = case _ of
        UENoToken -> "The unsubscribe token is missing"
        UESubscriptionDoesNotExist -> "This subscription is not in our database. Maybe you already unsubscribed?"
        UEOtherError msg -> msg
    withErrorHandler handler do
      mbToken <- lift $ getQueryParam "token"
      token <- except $ note UENoToken mbToken
      SubscriptionApi.unsubscribe token # flip runReaderT env >>= except
      lift $ send $ "You successfully unsubscribed"

  -- -- --

  use $ static "./dist"

apiErrHandler :: AppError -> Handler
apiErrHandler err = case err of
  JsonError msg -> unprocEntity $ show msg
  InvalidInput msg -> unprocEntity msg
  _ -> do
    log $ show err
    let status = 500
    setStatus status
    send { httpStatus: status, message: "Internal Server Error" }
  where
  unprocEntity (msg :: String) = do
    let status = 422
    setStatus status
    send { httpStatus: status, message: msg }

defErrHandler :: AppError -> Handler
defErrHandler = case _ of
  Unauthorized msg -> setStatus 401 *> send { error: msg }
  err -> do
    log $ show err
    setStatus 400
    send { error: show err }

runServer :: String -> Int -> Ref State -> Env -> Effect Http.Server
runServer host port stateRef env = do
  logL Warn "Server starting up..."
  httpServer <- listenHostHttp (server stateRef env) port host \_ ->
    logL Warn $ "Server listening on " <> host <> ":" <> show port
  pure httpServer

parseJsonReqBody :: forall a. ReadForeign a => ExceptT AppError HandlerM a
parseJsonReqBody = do
  fbody <- lift getBody'
  except $ lmap JsonError $ read fbody

withErrorHandler :: forall e. (e -> Handler) -> ExceptT e HandlerM Unit -> Handler
withErrorHandler errorHandler handler = do
  result <- runExceptT handler
  case result of
    Left err -> errorHandler err
    Right _ -> pure unit

ensureAuthorized :: ExceptT AppError HandlerM Unit
ensureAuthorized = do
  mbAuth <- lift $ getRequestHeader "Authorization"
  sauth <- except $ note (Unauthorized "No authorization header") mbAuth
  token <- case S.stripPrefix (S.Pattern "Bearer ") sauth of
    Nothing -> throwError $ Unauthorized "Malformed authorization header"
    Just t -> pure t
  result <- liftEffect $ jwtVerify token
  void $ except $ lmap Unauthorized result
