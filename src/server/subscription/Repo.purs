module WelcomeEmail.Server.Subscription.Repo where

import Prelude

import Control.Monad.Except (ExceptT, except, runExceptT)
import Data.Array as A
import Data.Either (Either(..), note)
import Data.Generic.Rep (class Generic)
import Data.JSDate (fromTime, getTime)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Traversable (traverse)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\))
import Effect.Aff (try)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref (Ref, modify_, new, read, write)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile, writeTextFile)
import Simple.JSON (readJSON_)
import WelcomeEmail.Server.Subscription.Entities (ChangeType(..), Frequency(..), Id, LatLng, Subscription)
import WelcomeEmail.Shared.Util (writeJSONPretty)


data Error
  = OtherError String

derive instance Eq Error
derive instance Generic Error _
instance Show Error where show = genericShow


class Repo repo where
  get :: forall m. MonadAff m => Id -> repo -> m (Either Error Subscription)
  getAll :: forall m. MonadAff m => repo -> m (Either Error (Array Subscription))
  create :: forall m. MonadAff m => Subscription -> repo -> m (Either Error Unit)
  update :: forall m. MonadAff m => Subscription -> repo -> m (Either Error Unit)
  delete :: forall m. MonadAff m => Id -> repo -> m (Either Error Unit)


defaultFileRepo = FileRepo "subscriptions.json" :: FileRepo

newtype FileRepo = FileRepo String

loadSubscriptions :: forall m. MonadAff m => String -> ExceptT Error m (Array Subscription)
loadSubscriptions fn = do
  res <- liftAff $ try $ readTextFile UTF8 fn
  let content = case res of
                  Left _ -> "[]"
                  Right c -> c
  bsubs :: Array BoundarySubscription <- except $ note (OtherError "Subscriptions could not be loaded: json error") $ readJSON_ content
  except $ note (OtherError "Subscriptions could not be loaded: from boundary error") $ traverse fromBoundarySubscription bsubs

saveSubscriptions :: forall m. MonadAff m => Array Subscription -> String -> ExceptT Error m Unit
saveSubscriptions subs fn = do
  liftAff $ writeTextFile UTF8 fn $ writeJSONPretty 2 $ map toBoundarySubscription subs
  pure unit

instance Repo FileRepo where
  get id' (FileRepo fn) = runExceptT do
    subs <- loadSubscriptions fn
    sub <- except $ note (OtherError "Subscription not found") $ A.find (\{ id } -> id == id') subs
    pure sub
  getAll (FileRepo fn) = runExceptT do
    loadSubscriptions fn
  create sub (FileRepo fn) = runExceptT do
    subs <- loadSubscriptions fn
    saveSubscriptions (A.snoc subs sub) fn
  update sub (FileRepo fn) = runExceptT do
    subs <- loadSubscriptions fn
    idx <- except $ note (OtherError "UpdateError: Subscription not found") $ A.findIndex (\{ id } -> id == sub.id) subs
    newSubs <- except $ note (OtherError "UpdateError: Index out of bounds") $ A.updateAt idx sub subs
    saveSubscriptions newSubs fn
  delete id' (FileRepo fn) = runExceptT do
    subs <- loadSubscriptions fn
    idx <- except $ note (OtherError "DeleteError: Subscription not found") $ A.findIndex (\{ id } -> id == id') subs
    newSubs <- except $ note (OtherError "DeleteError: Index out of bounds") $ A.deleteAt idx subs
    saveSubscriptions newSubs fn

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


newtype Mock = Mock (Ref (Array Subscription))

mkMock :: forall m. MonadEffect m => Array Subscription -> m Mock
mkMock subs = liftEffect $ new subs >>= pure <<< Mock

mockRepoContent :: forall m. MonadEffect m => Mock -> m (Array Subscription)
mockRepoContent (Mock mockRepoRef) = liftEffect $ read mockRepoRef

instance Repo Mock where
  get id' (Mock repoRef) = do
    subs <- liftEffect $ read repoRef
    pure $ note (OtherError "Subscription not found") $ A.find (\{ id } -> id == id') subs
  getAll (Mock repoRef) = do
    subs <- liftEffect $ read repoRef
    pure $ Right subs
  create sub (Mock repoRef) = do
    liftEffect $ modify_ (flip A.snoc sub) repoRef
    pure $ Right unit
  update sub (Mock repoRef) = runExceptT do
    subs <- liftEffect $ read repoRef
    idx <- except $ note (OtherError "UpdateError: Subscription not found") $ A.findIndex (\{ id } -> id == sub.id) subs
    newSubs <- except $ note (OtherError "UpdateError: Index out of bounds") $ A.updateAt idx sub subs
    liftEffect $ write newSubs repoRef
  delete id' (Mock repoRef) = runExceptT do
    subs <- liftEffect $ read repoRef
    idx <- except $ note (OtherError "DeleteError: Subscription not found") $ A.findIndex (\{ id } -> id == id') subs
    newSubs <- except $ note (OtherError "DeleteError: Index out of bounds") $ A.deleteAt idx subs
    liftEffect $ write newSubs repoRef

