module WelcomeEmail.Server.Services.Ofdb where

import Prelude

import Control.Monad.Except (ExceptT, except, withExceptT)
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.JSDate (JSDate)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Effect.Aff.Class (class MonadAff)
import WelcomeEmail.Server.Services.OfdbApi (class OfdbApi, defaultRcQuery, getEntriesRecentlyChanged)


data Error
  = OtherError String

derive instance Eq Error
derive instance Generic Error _
instance Show Error where show = genericShow

class Ofdb ofdb where
  recentlyChanged :: forall m. MonadAff m => ofdb -> ExceptT Error m (Array EntryChange)


updateFeed :: forall m ofdbApi.
  MonadAff m => OfdbApi ofdbApi =>
  ofdbApi -> ExceptT Error m Unit
updateFeed ofdbApi = do
  entries <- getEntriesRecentlyChanged defaultRcQuery { withRatings = Just true } ofdbApi # withExceptT (OtherError <<< show)
  pure unit

newtype Mock = Mock (Either Error (Array EntryChange))

instance Ofdb Mock where
  recentlyChanged (Mock res) = except res


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
