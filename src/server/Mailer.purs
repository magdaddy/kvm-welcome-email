module WelcomeEmail.Server.Mailer where

import Prelude

import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Effect.Aff.Class (class MonadAff)


data MailerError
  = Other String

derive instance Eq MailerError
derive instance Generic MailerError _
instance Show MailerError where show = genericShow

type Email =
  { from :: String
  , to :: Array String
  , subject :: String
  , body :: String
  }

class Mailer mailer where
  sendEmail :: forall m. MonadAff m => Email -> mailer -> m (Either MailerError Unit)


newtype MockMailer = MockMailer (Either MailerError Unit)

instance Mailer MockMailer where
  sendEmail _ (MockMailer res) = pure res

