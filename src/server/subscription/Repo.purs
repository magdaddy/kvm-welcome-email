module WelcomeEmail.Server.Subscription.Repo where

import Prelude

import Control.Monad.Except (ExceptT, withExceptT)
import Data.Generic.Rep (class Generic)
import Data.JSDate (fromTime, getTime)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import WelcomeEmail.Server.Services.CrudRepo (class CrudRepo, FileRepo(..))
import WelcomeEmail.Server.Services.CrudRepo as CrudRepo
import WelcomeEmail.Server.Subscription.Entities (ChangeType(..), Frequency(..), Id, LatLng, Subscription)


data Error
  = OtherError String

derive instance Eq Error
derive instance Generic Error _
instance Show Error where show = genericShow


class Repo repo where
  read :: forall m. MonadAff m =>
    Id -> repo -> ExceptT Error m Subscription
  readAll :: forall m. MonadAff m =>
    repo -> ExceptT Error m (Array Subscription)
  create :: forall m. MonadAff m =>
    Subscription -> repo -> ExceptT Error m Unit
  update :: forall m. MonadAff m =>
    Subscription -> repo -> ExceptT Error m Unit
  delete :: forall m. MonadAff m =>
    Id -> repo -> ExceptT Error m Unit


defaultFileRepo = FileRepo "data/subscriptions.json" :: FileRepo

instance CrudRepo.HasId NSub Id where
  id (NSub sub) = sub.id

instance CrudRepo.SerDe NSub BoundarySubscription where
  ser = toBoundarySubscription <<< unwrap
  deSer = map wrap <<< fromBoundarySubscription

instance CrudRepo FileRepo NSub Id => Repo FileRepo where
  read id repo = CrudRepo.read id repo # withExceptT (OtherError <<< show) >>= pure <<< unwrap
  readAll repo = CrudRepo.readAll repo # withExceptT (OtherError <<< show) >>= pure <<< map unwrap
  create sub repo = CrudRepo.create (wrap sub) repo # withExceptT (OtherError <<< show)
  update sub repo = CrudRepo.update (wrap sub) repo # withExceptT (OtherError <<< show)
  delete id repo = CrudRepo.delete id repo # withExceptT (OtherError <<< show)

type BoundarySubscription =
  { id :: String
  , email :: String
  , bbox :: { a :: LatLng, b :: LatLng }
  , tags :: Array String
  , frequency :: String
  , changeType :: String
  , confirmed :: Boolean
  , secret :: String
  , lastSent :: Number
  }

newtype NSub = NSub Subscription
derive instance Newtype NSub _

fromBoundarySubscription :: BoundarySubscription -> Maybe Subscription
fromBoundarySubscription { id, email, bbox, tags, frequency, changeType, confirmed, secret, lastSent } = do
  let bbox' = bbox.a /\ bbox.b
  frequency' <- fromBoundaryFrequency frequency
  changeType' <- fromBoundaryChangeType changeType
  let lastSent' = fromTime lastSent
  pure { id, email, bbox: bbox', tags, frequency: frequency', changeType: changeType', confirmed, secret, lastSent: lastSent' }

toBoundarySubscription :: Subscription -> BoundarySubscription
toBoundarySubscription { id, email, bbox, tags, frequency, changeType, confirmed, secret, lastSent } =
  { id
  , email
  , bbox: { a: fst bbox, b: snd bbox }
  , tags
  , frequency: toBoundaryFrequency frequency
  , changeType: toBoundaryChangeType changeType
  , confirmed
  , secret
  , lastSent: getTime lastSent
  }

fromBoundaryFrequency :: String -> Maybe Frequency
fromBoundaryFrequency = case _ of
  "hour" -> Just Hour
  "day" -> Just Day
  "week" -> Just Week
  _ -> Nothing

toBoundaryFrequency :: Frequency -> String
toBoundaryFrequency = case _ of
  Hour -> "hour"
  Day -> "day"
  Week -> "week"

fromBoundaryChangeType :: String -> Maybe ChangeType
fromBoundaryChangeType = case _ of
  "new" -> Just NewEntries
  "all" -> Just AllEntries
  _ -> Nothing

toBoundaryChangeType :: ChangeType -> String
toBoundaryChangeType = case _ of
  NewEntries -> "new"
  AllEntries -> "all"


type Mock = CrudRepo.Mock NSub

mkMock :: forall m. MonadEffect m => Array Subscription -> m Mock
mkMock subs = CrudRepo.mkMock $ map wrap subs

mockRepoContent :: forall m. MonadEffect m => Mock -> m (Array Subscription)
mockRepoContent crudMock = CrudRepo.mockRepoContent crudMock >>= pure <<< map unwrap

instance Repo (CrudRepo.Mock NSub) where
  read id repo = CrudRepo.read id repo # withExceptT (OtherError <<< show) >>= pure <<< unwrap
  readAll repo = CrudRepo.readAll repo # withExceptT (OtherError <<< show) >>= pure <<< map unwrap
  create sub repo = CrudRepo.create (wrap sub) repo # withExceptT (OtherError <<< show)
  update sub repo = CrudRepo.update (wrap sub) repo # withExceptT (OtherError <<< show)
  delete id repo = CrudRepo.delete id repo # withExceptT (OtherError <<< show)

