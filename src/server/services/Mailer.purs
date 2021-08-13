module WelcomeEmail.Server.Services.Mailer where

import Prelude

import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Effect.Aff.Class (class MonadAff)


data Error
  = OtherError String

derive instance Eq Error
derive instance Generic Error _
instance Show Error where show = genericShow

type Email =
  { from :: String
  , to :: Array String
  , subject :: String
  , body :: String
  }

class Mailer mailer where
  sendEmail :: forall m. MonadAff m => Email -> mailer -> m (Either Error Unit)


newtype MockMailer = MockMailer (Either Error Unit)

instance Mailer MockMailer where
  sendEmail _ (MockMailer res) = pure res

