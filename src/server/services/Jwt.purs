module KvmMail.Server.Services.Jwt where

import ThisPrelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)


data Error
  = Other String

derive instance Eq Error
derive instance Generic Error _
instance Show Error where show = genericShow


class Jwt mailer where
  verify :: forall m. MonadEffect m =>  mailer -> m (Either Error Unit)



