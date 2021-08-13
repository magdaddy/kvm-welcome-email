module WelcomeEmail.Server.Subscription.Ofdb where

import Prelude

import Data.Either (Either)
import Data.Maybe (Maybe)
import Effect.Aff.Class (class MonadAff)


data OfdbError
  = Other String


class Ofdb ofdb where
  getRecentlyChanged :: forall m. MonadAff m => ofdb -> m (Either OfdbError (Array Entry))


newtype MockOfdb = MockOfdb (Either OfdbError (Array Entry))

instance Ofdb MockOfdb where
  getRecentlyChanged (MockOfdb res) = pure res


type Entry
  = { id :: String
    , title :: String
    , created :: Number
    , version :: Int
    , contact_name :: Maybe String
    , email :: Maybe String
    , country :: Maybe String
    , lat :: Number
    , lng :: Number
    }
