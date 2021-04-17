module Nmailer where

import Prelude

import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff, try)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (message, throw)
import NodeMailer (TransportConfig, Message, createTransporter, sendMail)
import WelcomeEmail.Server.Data (AppError(..))
import WelcomeEmail.Server.Settings (loadSettings)
import WelcomeEmail.Server.Template (loadTemplate)
import WelcomeEmail.Shared.Boundary (Email)
import WelcomeEmail.Shared.Marked (markedS)
import WelcomeEmail.Shared.Template (expand)

-- tsend :: Effect Unit
-- tsend = do
--   s <- loadState
--   case s of
--     Left err -> throw err
--     Right ss -> launchAff_ $ do
--       result <- send ss.nodeMailer
--       case result of
--         Left err -> do
--           liftEffect $ log $ message err
--         Right _ -> liftEffect $ log "send successful"

send :: TransportConfig -> Message -> Aff (Either AppError Unit)
send conf msg = do
  trans <- liftEffect $ createTransporter conf
  result <- try $ sendMail msg trans
  pure $ lmap NodeMailerError result


mkMessage :: String -> Email -> Message
mkMessage to email =
  { from: "magnus.herold@gmail.com"
  , to: [ to ]
  , subject: email.subject
  , text: email.body
  , html: Just $ markedS email.body
  , attachments: []
  , cc: []
  , bcc: []
  }

sendTestMail :: String -> Aff (Either AppError Unit)
sendTestMail addr = do
  settings <- liftEffect loadSettings
  templ <- liftEffect loadTemplate
  let email = expand settings.defaultEntry templ
  let msg = mkMessage addr email
  send settings.nodeMailer msg
