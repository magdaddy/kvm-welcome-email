module WelcomeEmail.Server.Express where

import Prelude

import Control.Monad.Except (ExceptT, except, lift, runExceptT)
import Data.Array (find)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (error, forkAff, killFiber)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Nmailer (sendTestMail)
import Node.Express.App (App, get, listenHttp, post, use, useExternal)
import Node.Express.Handler (Handler, HandlerM)
import Node.Express.Middleware.Static (static)
import Node.Express.Request (getBody')
import Node.Express.Response (send, setStatus)
import Node.Express.Types (Middleware)
import Simple.JSON (class ReadForeign, read, writeJSON)
import WelcomeEmail.Server.Core (theloop)
import WelcomeEmail.Server.Data (AppError(..))
import WelcomeEmail.Server.Log (LogLevel(..), log, logL)
import WelcomeEmail.Server.Settings (loadSettings, saveSettings)
import WelcomeEmail.Server.Template (loadTemplate, saveTemplate)
import WelcomeEmail.Server.Util (getUsers, jwtSign, tokenSecret)
import WelcomeEmail.Server.Winston (morgan)
import WelcomeEmail.Shared.Boundary (LoginData, Settings, TestMailPayload)
import WelcomeEmail.Shared.State (State)
import WelcomeEmail.Shared.Template (EmailTemplate)

foreign import cors :: Middleware
foreign import text :: Middleware
foreign import json :: Middleware

port = 4000 :: Int

server :: Ref State -> App
server stateRef = do
  useExternal morgan
  useExternal cors
  useExternal text
  useExternal json

  get "/" do
    send "Hello, World!"

  post "/login" $ do
    withErrorHandler defErrHandler do
      loginData :: LoginData <- parseJsonReqBody
      result <- liftEffect $ getUsers
      users <- except $ lmap (\err -> OtherError $ "Something's wrong with the users... this should never happen." <> err) result
      case find (\u -> u.name == loginData.username && u.pwd == loginData.password) users of
        Nothing -> lift $ send { error: "Login failed, wrong credentials." }
        Just _ -> do
          let jwt = jwtSign {} tokenSecret { subject: loginData.username, expiresIn: "1w" }
          lift $ send { token: jwt }

  post "/sendtestmail" do
    withErrorHandler defErrHandler do
      pl :: TestMailPayload <- parseJsonReqBody
      liftEffect $ log $ show pl
      result <- liftAff $ sendTestMail pl.emailAddr
      except result
      lift $ send unit

  get "/settings" do
    settings <- liftEffect loadSettings
    send $ writeJSON settings

  post "/settings" do
    withErrorHandler defErrHandler do
      settings :: Settings <- parseJsonReqBody
      liftEffect $ saveSettings settings
      lift $ send unit

  get "/template" do
    email <- liftEffect loadTemplate
    send $ writeJSON email

  post "/template" do
    withErrorHandler defErrHandler do
      templ :: EmailTemplate <- parseJsonReqBody
      liftEffect $ saveTemplate templ
      lift $ send unit

  get "/serverstate" do
    state <- liftEffect $ Ref.read stateRef
    case state.running of
      Nothing -> send $ writeJSON { isRunning: false }
      Just _fiber -> send $ writeJSON { isRunning: true }

  post "/togglerunning" do
    state <- liftEffect $ Ref.read stateRef
    case state.running of
      Nothing -> do
        fiber <- liftAff $ forkAff $ theloop stateRef
        liftEffect $ Ref.modify_ _ { running = Just fiber } stateRef
        liftEffect $ logL Verbose "started running"
        send $ writeJSON { isRunning: true }
      Just fiber -> do
        liftAff $ killFiber (error "stop requested") fiber
        liftEffect $ Ref.modify_ _ { running = Nothing } stateRef
        liftEffect $ logL Verbose "stopped running"
        send $ writeJSON { isRunning: false }

  use $ static "./public"


defErrHandler :: AppError -> Handler
defErrHandler err = do
  liftEffect $ log $ show err
  setStatus 400
  send { error: show err }

runServer :: Ref State -> Effect Unit
runServer stateRef = do
  log "Starting up..."
  void $ listenHttp (server stateRef) port \_ ->
    log $ "Listening on " <> show port
  pure unit

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
