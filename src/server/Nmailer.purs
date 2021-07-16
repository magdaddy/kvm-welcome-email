module Nmailer where

import Prelude

import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff, try)
import Effect.Class (liftEffect)
import NodeMailer (TransportConfig, Message, createTransporter, sendMail)
import Record as R
import Type.Proxy (Proxy(..))
import WelcomeEmail.Server.Data (AppError(..))
import WelcomeEmail.Server.Settings (loadSettings)
import WelcomeEmail.Server.Template (loadTemplate)
import WelcomeEmail.Server.Util (NodeEnv(..), getNodeEnv)
import WelcomeEmail.Shared.Boundary (Email)
import WelcomeEmail.Shared.Marked (markedS)
import WelcomeEmail.Shared.Template (expand)


send :: TransportConfig () -> Message -> Aff (Either AppError Unit)
send conf msg = do
  nodeEnv <- liftEffect getNodeEnv
  trans <- case nodeEnv of
    Production -> liftEffect $ createTransporter conf
    _ -> do
      let conf' = R.insert (Proxy :: _ "tls") { rejectUnauthorized: false } conf
      liftEffect $ createTransporter conf'
  result <- try $ sendMail msg trans
  pure $ lmap NodeMailerError result


mkMessage :: String -> String -> Email -> Message
mkMessage from to email =
  { from: from
  , to: [ to ]
  , subject: email.subject
  , text: email.body
  , html: Just $ markedS email.body
  , attachments: []
  , cc: []
  , bcc: []
  }

sendTestMail :: String -> Aff (Either AppError Unit)
sendTestMail toAddr = do
  settings <- liftEffect loadSettings
  templ <- liftEffect loadTemplate
  let email = expand settings.defaultEntry templ
  let msg = mkMessage settings.senderAddress toAddr email
  _nodeEnv <- liftEffect getNodeEnv
  send settings.nodeMailer msg
