module WelcomeEmail.Server.Services.OfdbApi where

import Prelude

import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import Control.Monad.Except (ExceptT, except, withExceptT)
import Data.Array as A
import Data.Bifunctor (rmap)
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.JSDate (JSDate, fromTime)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.String as S
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Foreign (renderForeignError)
import MagLibs.DateFns as DFN
import Simple.JSON (readJSON)
import WelcomeEmail.Server.Core.Entities (Entry)
import WelcomeEmail.Server.OfdbApi (makeQueryStr)


data Error
  = OtherError String

derive instance Eq Error
derive instance Generic Error _
instance Show Error where show = genericShow

type RcQuery =
  { since :: Maybe JSDate
  , until :: Maybe JSDate
  , withRatings :: Maybe Boolean
  , limit :: Maybe Int
  , offset :: Maybe Int
  }

class OfdbApi ofdbApi where
  getEntriesRecentlyChanged :: forall m. MonadAff m => RcQuery -> ofdbApi -> ExceptT Error m (Array Entry)


newtype OfdbApiRest = OfdbApiRest { baseUrl :: String }

instance OfdbApi OfdbApiRest where
  getEntriesRecentlyChanged query (OfdbApiRest { baseUrl }) = do
    let route = "/entries/recently-changed"
    let url = S.joinWith "?" [ baseUrl <> route, rcQueryToQueryStr query ]
    liftEffect $ log url
    response <- AX.get ResponseFormat.string url # liftAff >>= except # withExceptT (OtherError <<< AX.printError)
    withExceptT (OtherError <<< S.joinWith "\n" <<< A.fromFoldable <<< (map renderForeignError)) $ except $ rmap (map fromBEntry) $ readJSON response.body

newtype Mock = Mock (Either Error (Array Entry))

instance OfdbApi Mock where
  getEntriesRecentlyChanged _query (Mock res) = except res


rcQueryToQueryStr :: RcQuery -> String
rcQueryToQueryStr query = makeQueryStr queryArr
  where
  queryArr = A.mapMaybe func rcQueryKeys :: Array (String /\ String)
  func :: (String /\ _) -> Maybe (String /\ String)
  func (key /\ ex) = case ex query of
    Nothing -> Nothing
    Just exq -> Just (key /\ exq)


defaultRcQuery :: RcQuery
defaultRcQuery =
  { since: Nothing
  , until: Nothing
  , withRatings: Nothing
  , limit: Nothing
  , offset: Nothing
  }

rcQueryKeys =
  [ "since" /\ map (DFN.format "t") <<< _.since
  , "until" /\ map (DFN.format "t") <<< _.until
  , "withRatings" /\ map show <<< _.withRatings
  , "limit" /\ map show <<< _.limit
  , "offset" /\ map show <<< _.offset
  ] :: Array (String /\ (RcQuery -> Maybe String))

type BEntry
  = { id :: String
    , created :: Number
    , version :: Int
    , title :: String
    , description :: String
    , lat :: Number
    , lng :: Number
    , street :: Maybe String
    , zip :: Maybe String
    , city :: Maybe String
    , country :: Maybe String
    , state :: Maybe String
    , contact_name :: Maybe String
    , email :: Maybe String
    , telephone :: Maybe String
    , homepage :: Maybe String
    , opening_hours :: Maybe String
    -- , founded_on
    , categories :: Array String
    }

fromBEntry :: BEntry -> Entry
fromBEntry
  { id
  , created
  , version
  , title
  -- , description
  , lat
  , lng
  -- , street
  -- , zip
  -- , city
  , country
  -- , state
  , contact_name
  , email
  -- , telephone
  -- , homepage
  -- , opening_hours
  -- , founded_on
  -- , categories
  } =
  { id
  , created: fromTime created
  , version
  , title
  , contactName: contact_name
  , email
  , country
  , lat
  , lng
  }
