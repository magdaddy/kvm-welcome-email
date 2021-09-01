module WelcomeEmail.Server.Services.OfdbApi where

import ThisPrelude

import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import Data.Array as A
import Data.Bifunctor (rmap)
import Data.Generic.Rep (class Generic)
import Data.JSDate (JSDate)
import Data.Show.Generic (genericShow)
import Data.String as S
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Console (log)
import Foreign (renderForeignError)
import MagLibs.DateFns as DFN
import Simple.JSON (readJSON)
import WelcomeEmail.Server.OfdbApi (makeQueryStr)
import WelcomeEmail.Shared.Entry (Entry, fromBEntry)


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
    -- liftEffect $ log url
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
  , "with_ratings" /\ map show <<< _.withRatings
  , "limit" /\ map show <<< _.limit
  , "offset" /\ map show <<< _.offset
  ] :: Array (String /\ (RcQuery -> Maybe String))


