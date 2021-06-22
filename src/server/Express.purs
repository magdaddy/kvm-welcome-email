module WelcomeEmail.Server.Express where

import Prelude

import Control.Monad.Except (ExceptT, except, lift, runExceptT, throwError)
import Data.Array (find)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), note)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (error, forkAff, killFiber)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Nmailer (sendTestMail)
import Node.Express.App (App, get, listenHttp, post, use, useExternal)
import Node.Express.Handler (Handler, HandlerM)
import Node.Express.Middleware.Static (static)
import Node.Express.Request (getBody', hasType)
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
      users <- EHandlerM $ except $ lmap (\err -> OtherError $ "Something's wrong with the users... this should never happen." <> err) result
      case find (\u -> u.name == loginData.username && u.pwd == loginData.password) users of
        Nothing -> liftHM $ sendError "Login failed, wrong credentials."
        Just _ -> do
          let jwt = jwtSign {} tokenSecret { subject: loginData.username, expiresIn: "1h" }
          liftHM $ send $ writeJSON { token: jwt }


  -- post "/login" do
  --   parseRes <- parseReqBody
  --   case parseRes of
  --     Left err -> do
  --       liftEffect $ log $ show err
  --       sendError $ show err
  --     Right (loginData :: LoginData) -> do
  --       result <- liftEffect $ getUsers
  --       case result of
  --         Left err -> do
  --           let msg = "Something's wrong with the users... this should never happen." <> err
  --           liftEffect $ logL Error msg
  --           sendError msg
  --         Right users -> case find (\u -> u.name == loginData.username && u.pwd == loginData.password) users of
  --           Nothing -> sendError "Login failed, wrong credentials."
  --           Just _ -> do
  --             let jwt = jwtSign {} tokenSecret { subject: loginData.username, expiresIn: "1h" }
  --             send $ writeJSON { token: jwt }

  post "/sendtestmail" do
    withErrorHandler defErrHandler do
      pl :: TestMailPayload <- parseJsonReqBody
      liftEffect $ log $ show pl
      EHandlerM $ (liftAff $ sendTestMail pl.emailAddr) >>= except
      liftHM $ send unit

  -- post "/sendtestmail" do
  --   bodF <- getBody'
  --   bod <- runExceptT $ readString bodF
  --   liftEffect $ log $ show bod
  --   case bod of
  --     Left err -> send { error: show err }
  --     Right body -> case readJSON body of
  --       Left err -> send { error: show err }
  --       Right (pl :: TestMailPayload) -> do
  --         liftEffect $ log $ show pl
  --         result <- liftAff $ sendTestMail pl.emailAddr
  --         case result of
  --           Left err -> send { error: show err }
  --           Right _ -> send unit

  get "/settings" do
    settings <- liftEffect loadSettings
    send $ writeJSON settings

  post "/settings" do
    withErrorHandler defErrHandler do
      settings :: Settings <- parseJsonReqBody
      liftEffect $ saveSettings settings
      liftHM $ send unit

  get "/template" do
    email <- liftEffect loadTemplate
    send $ writeJSON email

  post "/template" do
    withErrorHandler defErrHandler do
      templ :: EmailTemplate <- parseJsonReqBody
      liftEffect $ saveTemplate templ
      liftHM $ send unit

  get "/serverstate" do
    state <- liftEffect $ Ref.read stateRef
    case state.running of
      Nothing -> send $ writeJSON { isRunning: false }
      Just fiber -> send $ writeJSON { isRunning: true }

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


newtype EHandlerM e a = EHandlerM (ExceptT e HandlerM a)

derive newtype instance functorEHandlerM :: Functor (EHandlerM e)
derive newtype instance applyEHandlerM :: Apply (EHandlerM e)
derive newtype instance applicativeEHandlerM :: Applicative (EHandlerM e)
derive newtype instance bindEHandlerM :: Bind (EHandlerM e)
derive newtype instance monadEHandlerM :: Monad (EHandlerM e)
derive newtype instance monadEffectEHandlerM :: MonadEffect (EHandlerM e)
derive newtype instance monadAffEHandlerM :: MonadAff (EHandlerM e)

liftHM :: forall e a. HandlerM a -> EHandlerM e a
liftHM = EHandlerM <<< lift

liftMaybeHM :: forall e a. HandlerM (Maybe a) -> e -> EHandlerM e a
liftMaybeHM h err = EHandlerM do
  mbVal <- lift h
  except $ note err mbVal

parseJsonReqBody :: forall a. ReadForeign a => EHandlerM AppError a
parseJsonReqBody = EHandlerM do
  isContentTypeJson <- lift $ hasType "application/json"
  when (not isContentTypeJson) $ throwError $ OtherError "Content type of request is not application/json."
  fbody <- lift getBody'
  -- traceM fbody
  except $ lmap JsonError $ read fbody

withErrorHandler :: forall e. (e -> Handler) -> EHandlerM e Unit -> Handler
withErrorHandler errorHandler (EHandlerM handler) = do
  result <- runExceptT handler
  case result of
    Left err -> errorHandler err
    Right _ -> pure unit
