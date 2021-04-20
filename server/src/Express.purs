module WelcomeEmail.Server.Express where

import Prelude

import Control.Monad.Except (runExceptT)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Foreign (readString)
import Nmailer (sendTestMail)
import Node.Express.App (App, get, listenHttp, post, use, useExternal)
import Node.Express.Middleware.Static (static)
import Node.Express.Request (getBody')
import Node.Express.Response (send, sendJson, setStatus)
import Node.Express.Types (Middleware)
import Simple.JSON (readJSON, writeJSON)
import State (encodeJsonState)
import StateIO (loadState)
import WelcomeEmail.Server.Log (log)
import WelcomeEmail.Server.Settings (loadSettings, saveSettings)
import WelcomeEmail.Server.Template (loadTemplate, saveTemplate)
import WelcomeEmail.Shared.Boundary (Settings, TestMailPayload)
import WelcomeEmail.Shared.Template (EmailTemplate)

foreign import cors :: Middleware
foreign import text :: Middleware

port = 4000 :: Int

server :: App
server = do
  useExternal cors
  useExternal text

  get "/" do
    send "Hello, World!"

  post "/sendtestmail" do
    bodF <- getBody'
    bod <- runExceptT $ readString bodF
    liftEffect $ log $ show bod
    case bod of
      Left err -> send { error: show err }
      Right body -> case readJSON body of
        Left err -> send { error: show err }
        Right (pl :: TestMailPayload) -> do
          liftEffect $ log $ show pl
          result <- liftAff $ sendTestMail pl.emailAddr
          case result of
            Left err -> send { error: show err }
            Right _ -> send unit

  get "/settings" do
    settings <- liftEffect loadSettings
    send $ writeJSON settings

  post "/settings" do
    bodF <- getBody'
    bod <- runExceptT $ readString bodF
    liftEffect $ log $ show bod
    case bod of
      Left err -> liftEffect $ log $ show err
      Right body -> case readJSON body of
        Left err -> liftEffect $ log $ "json: " <> show err
        Right (settings :: Settings) -> liftEffect $ saveSettings settings
    send unit

  get "/template" do
    email <- liftEffect loadTemplate
    send $ writeJSON email

  post "/template" do
    liftEffect $ log "savetemplate"
    bodF <- getBody'
    bod <- runExceptT $ readString bodF
    liftEffect $ log $ show bod
    case bod of
      Left err -> liftEffect $ throw $ show err
      Right body -> case readJSON body of
        Left err -> liftEffect $ log $ "json: " <> show err
        Right (templ :: EmailTemplate) -> liftEffect $ saveTemplate templ
    send unit

  get "/serverstate" do
    resState <- liftEffect loadState
    case resState of
      Left err -> do
        liftEffect $ log $ err
        setStatus 500
        send err
      Right json -> sendJson $ encodeJsonState json

  use $ static "./public"


runServer :: Effect Unit
runServer = do
  log "Starting up..."
  void $ listenHttp server port \_ ->
    log $ "Listening on " <> show port
  pure unit

