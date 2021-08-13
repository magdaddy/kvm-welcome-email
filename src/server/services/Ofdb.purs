module WelcomeEmail.Server.Services.Ofdb where

import Prelude

import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.JSDate (JSDate)
import Data.Maybe (Maybe)
import Effect.Aff.Class (class MonadAff)


data Error
  = Other String

derive instance Eq Error
derive instance Generic Error _
instance Show Error where show = genericShow

class Ofdb ofdb where
  getRecentlyChanged :: forall m. MonadAff m => ofdb -> m (Either Error (Array EntryChange))


newtype Mock = Mock (Either Error (Array EntryChange))

instance Ofdb Mock where
  getRecentlyChanged (Mock res) = pure res


type EntryChange =
  { changed :: JSDate
  , entry :: RcEntry ()
  }

type RcEntry r =
  { id :: String
  , lat :: Number
  , lng :: Number
  | r
  }

type Entry
  = { id :: String
    , title :: String
    , created :: JSDate
    , version :: Int
    , contact_name :: Maybe String
    , email :: Maybe String
    , country :: Maybe String
    , lat :: Number
    , lng :: Number
    }
