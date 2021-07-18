module WelcomeEmail.Server.Express where

import Prelude

import Control.Monad.Except (ExceptT, except, lift, runExceptT, throwError)
import Data.Array (find)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), note)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), stripPrefix)
import Effect (Effect)
import Effect.Aff (error, forkAff, killFiber)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Nmailer (sendTestMail)
import Node.Express.App (App, get, listenHostHttp, post, use, useExternal)
import Node.Express.Handler (Handler, HandlerM)
import Node.Express.Middleware.Static (static)
import Node.Express.Request (getBody', getRequestHeader)
import Node.Express.Response (send, setStatus)
import Node.Express.Types (Middleware)
import Node.HTTP (Server) as Http
import Simple.JSON (class ReadForeign, read, writeJSON)
import WelcomeEmail.Server.Core (theloop)
import WelcomeEmail.Server.Data (AppError(..))
import WelcomeEmail.Server.LastLogs (loadLastLogs)
import WelcomeEmail.Server.Log (LogLevel(..), log, logL)
import WelcomeEmail.Server.Settings (loadSettings, saveSettings)
import WelcomeEmail.Server.Template (loadTemplate, saveTemplate)
import WelcomeEmail.Server.Util (getUsers, jwtSign, jwtVerify, tokenSecret)
import WelcomeEmail.Server.Winston (morgan)
import WelcomeEmail.Shared.Boundary (LoginData, Settings, TestMailPayload)
import WelcomeEmail.Shared.State (State)
import WelcomeEmail.Shared.Template (EmailTemplate)

foreign import cors :: Middleware
foreign import text :: Middleware
foreign import json :: Middleware

server :: Ref State -> App
server stateRef = do
  useExternal morgan
  useExternal cors
  useExternal text
  useExternal json

  post "/api/login" $ do
    withErrorHandler defErrHandler do
      loginData :: LoginData <- parseJsonReqBody
      result <- liftEffect $ getUsers
      users <- except $ lmap (\err -> OtherError $ "Something's wrong with the users... this should never happen." <> err) result
      case find (\u -> u.name == loginData.username && u.pwd == loginData.password) users of
        Nothing -> lift $ send { error: "Login failed, wrong credentials." }
        Just _ -> do
          let jwt = jwtSign {} tokenSecret { subject: loginData.username, expiresIn: "1w" }
          lift $ send { token: jwt }

  post "/api/sendtestmail" do
    withErrorHandler defErrHandler do
      ensureAuthorized
      pl :: TestMailPayload <- parseJsonReqBody
      liftEffect $ log $ show pl
      result <- liftAff $ sendTestMail pl.emailAddr
      except result
      lift $ send unit

  get "/api/settings" do
    withErrorHandler defErrHandler do
      ensureAuthorized
      settings <- liftEffect loadSettings
      lift $ send $ writeJSON settings

  post "/api/settings" do
    withErrorHandler defErrHandler do
      ensureAuthorized
      settings :: Settings <- parseJsonReqBody
      liftEffect $ saveSettings settings
      lift $ send unit

  get "/api/template" do
    withErrorHandler defErrHandler do
      ensureAuthorized
      email <- liftEffect loadTemplate
      lift $ send $ writeJSON email

  post "/api/template" do
    withErrorHandler defErrHandler do
      ensureAuthorized
      templ :: EmailTemplate <- parseJsonReqBody
      liftEffect $ saveTemplate templ
      lift $ send unit

  get "/api/serverstate" do
    withErrorHandler defErrHandler do
      ensureAuthorized
      state <- liftEffect $ Ref.read stateRef
      case state.running of
        Nothing -> lift $ send $ writeJSON { isRunning: false }
        Just _fiber -> lift $ send $ writeJSON { isRunning: true }

  post "/api/togglerunning" do
    withErrorHandler defErrHandler do
      ensureAuthorized
      state <- liftEffect $ Ref.read stateRef
      case state.running of
        Nothing -> do
          fiber <- liftAff $ forkAff $ theloop stateRef
          liftEffect $ Ref.modify_ _ { running = Just fiber } stateRef
          liftEffect $ logL Warn "Sending-service started"
          lift $ send $ writeJSON { isRunning: true }
        Just fiber -> do
          liftAff $ killFiber (error "stop requested") fiber
          liftEffect $ Ref.modify_ _ { running = Nothing } stateRef
          liftEffect $ logL Warn "Sending-service stopped"
          lift $ send $ writeJSON { isRunning: false }

  get "/api/lastlogs" do
    withErrorHandler defErrHandler do
      ensureAuthorized
      result <- liftAff $ runExceptT $ loadLastLogs 300
      lastLogs <- except result
      lift $ send $ lastLogs

  use $ static "./dist"


defErrHandler :: AppError -> Handler
defErrHandler = case _ of
  Unauthorized msg -> setStatus 401 *> send { error: msg }
  err -> do
    liftEffect $ log $ show err
    setStatus 400
    send { error: show err }

runServer :: String -> Int -> Ref State -> Effect Http.Server
runServer host port stateRef = do
  logL Warn "Server starting up..."
  httpServer <- listenHostHttp (server stateRef) port host \_ ->
    logL Warn $ "Server listening on " <> host <> ":" <> show port
  pure httpServer

sendError :: String -> Handler
sendError err = send (writeJSON { error: err })

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
  token <- case stripPrefix (Pattern "Bearer ") sauth of
    Nothing -> throwError $ Unauthorized "Malformed authorization header"
    Just t -> pure t
  result <- liftEffect $ jwtVerify token
  void $ except $ lmap Unauthorized result
