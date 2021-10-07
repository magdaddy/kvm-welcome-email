module KvmMail.Server.Subscription.Repo where

import ThisPrelude

import Data.Generic.Rep (class Generic)
import Data.JSDate (fromTime, getTime)
import Data.Show.Generic (genericShow)
import KvmMail.Server.Services.CrudRepo (class CrudRepo, FileRepo(..))
import KvmMail.Server.Services.CrudRepo as CrudRepo
import KvmMail.Server.Subscription.Entities (ChangeType(..), Frequency(..), Id, Lang(..), LatLng, Subscription, mkBBox, northEast, southWest)
import KvmMail.Shared.Boundary (class SerDe)

data Error
  = OtherError String
  | NotFoundError

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

instance SerDe NSub BoundarySubscription where
  ser = toBoundarySubscription <<< unwrap
  deSer = map wrap <<< fromBoundarySubscription

instance CrudRepo FileRepo NSub Id => Repo FileRepo where
  read id repo = CrudRepo.read id repo # withExceptT fromCrudRepoError >>= pure <<< unwrap
  readAll repo = CrudRepo.readAll repo # withExceptT fromCrudRepoError >>= pure <<< map unwrap
  create sub repo = CrudRepo.create (wrap sub) repo # withExceptT fromCrudRepoError
  update sub repo = CrudRepo.update (wrap sub) repo # withExceptT fromCrudRepoError
  delete id repo = CrudRepo.delete id repo # withExceptT fromCrudRepoError

fromCrudRepoError :: CrudRepo.Error -> Error
fromCrudRepoError CrudRepo.NotFoundError = NotFoundError
fromCrudRepoError e = OtherError $ show e

type BoundarySubscription =
  { id :: String
  , title :: String
  , email :: String
  , lang :: String
  , bbox :: { sw :: LatLng, ne :: LatLng }
  , tags :: Array String
  , frequency :: String
  , changeType :: String
  , confirmed :: Boolean
  , secret :: String
  , lastSent :: Number
  , created :: Number
  }

newtype NSub = NSub Subscription
derive instance Newtype NSub _

fromBoundarySubscription :: BoundarySubscription -> Maybe Subscription
fromBoundarySubscription { id, title, email, lang, bbox, tags, frequency, changeType, confirmed, secret, lastSent, created } = do
  let bbox' = mkBBox bbox.sw bbox.ne
  lang' <- fromBoundaryLang lang
  frequency' <- fromBoundaryFrequency frequency
  changeType' <- fromBoundaryChangeType changeType
  let lastSent' = fromTime lastSent
  let created' = fromTime created
  pure
    { id, title, email, lang: lang', bbox: bbox', tags, frequency: frequency'
    , changeType: changeType', confirmed, secret, lastSent: lastSent', created: created'
    }

toBoundarySubscription :: Subscription -> BoundarySubscription
toBoundarySubscription { id, title, email, lang, bbox, tags, frequency, changeType, confirmed, secret, lastSent, created } =
  { id
  , title
  , email
  , lang: toBoundaryLang lang
  , bbox: { sw: southWest bbox, ne: northEast bbox }
  , tags
  , frequency: toBoundaryFrequency frequency
  , changeType: toBoundaryChangeType changeType
  , confirmed
  , secret
  , lastSent: getTime lastSent
  , created: getTime created
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

fromBoundaryLang :: String -> Maybe Lang
fromBoundaryLang = case _ of
  "de" -> Just DE
  "en" -> Just EN
  _ -> Nothing

toBoundaryLang :: Lang -> String
toBoundaryLang = case _ of
  DE -> "de"
  EN -> "en"


type Mock = CrudRepo.Mock NSub

mkMock :: forall m. MonadEffect m => Array Subscription -> m Mock
mkMock subs = CrudRepo.mkMock $ map wrap subs

mockRepoContent :: forall m. MonadEffect m => Mock -> m (Array Subscription)
mockRepoContent crudMock = CrudRepo.mockRepoContent crudMock >>= pure <<< map unwrap

instance Repo (CrudRepo.Mock NSub) where
  read id repo = CrudRepo.read id repo # withExceptT fromCrudRepoError >>= pure <<< unwrap
  readAll repo = CrudRepo.readAll repo # withExceptT fromCrudRepoError >>= pure <<< map unwrap
  create sub repo = CrudRepo.create (wrap sub) repo # withExceptT fromCrudRepoError
  update sub repo = CrudRepo.update (wrap sub) repo # withExceptT fromCrudRepoError
  delete id repo = CrudRepo.delete id repo # withExceptT fromCrudRepoError

