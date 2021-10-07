module Nmailer where

import ThisPrelude

import Effect.Aff (try)
import NodeMailer (TransportConfig, Message, createTransporter, sendMail)
import Record as R
import Type.Proxy (Proxy(..))
import KvmMail.Server.Data (AppError(..))
import KvmMail.Server.Services.Mailer (class Mailer)
import KvmMail.Server.Services.Mailer as Mailer
import KvmMail.Server.Settings (loadSettings)
import KvmMail.Server.Template (loadTemplate)
import KvmMail.Server.Util (NodeEnv(..), getNodeEnv)
import KvmMail.Shared.Boundary (Email)
import KvmMail.Shared.Marked (markedS)
import KvmMail.Shared.Template (expand)


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


newtype NMailer = NMailer Unit

instance Mailer NMailer where
  sendEmail { from, to, subject, body } _ = do
    settings <- liftEffect $ loadSettings
    res <- liftAff $ send settings.nodeMailer
      { from, to, subject, text: body
      , html: Just $ markedS body
      , attachments: []
      , cc: []
      , bcc: []
      }
    pure $ lmap (Mailer.OtherError <<< show) res
