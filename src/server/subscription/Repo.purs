module WelcomeEmail.Server.Subscription.Repo where

import Prelude

import Control.Monad.Except (except, lift, runExceptT)
import Data.Array (find, snoc)
import Data.Either (Either(..), note)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Traversable (traverse)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff, try)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref (Ref, modify_, new, read)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile, writeTextFile)
import Simple.JSON (readJSON_)
import WelcomeEmail.Server.Subscription.Entities (ChangeType(..), Frequency(..), Id, LatLng, Subscription)
import WelcomeEmail.Shared.Util (writeJSONPretty)


data RepoError
  = Other String

derive instance Eq RepoError
derive instance Generic RepoError _
instance Show RepoError where show = genericShow


class Repo repo where
  get :: Id -> repo -> Aff (Either RepoError Subscription)
  create :: Subscription -> repo -> Aff (Either RepoError Unit)


defaultFileRepo = FileRepo "subscriptions.json" :: FileRepo

newtype FileRepo = FileRepo String

loadSubscriptions :: String -> Aff (Either RepoError (Array Subscription))
loadSubscriptions fn = runExceptT do
  res <- lift $ try $ readTextFile UTF8 fn
  let content = case res of
                  Left _ -> "[]"
                  Right c -> c
  bsubs :: Array BoundarySubscription <- except $ note (Other "Subscriptions could not be loaded: json error") $ readJSON_ content
  except $ note (Other "Subscriptions could not be loaded: from boundary error") $ traverse fromBoundarySubscription bsubs

saveSubscriptions :: Array Subscription -> String -> Aff (Either RepoError Unit)
saveSubscriptions subs fn = do
  writeTextFile UTF8 fn $ writeJSONPretty 2 $ map toBoundarySubscription subs
  pure $ Right unit

instance Repo FileRepo where
  get id' (FileRepo fn) = runExceptT do
    res <- lift $ loadSubscriptions fn
    subs <- except res
    sub <- except $ note (Other "Subscription not found") $ find (\{ id } -> id == id') subs
    pure sub
  create sub (FileRepo fn) = runExceptT do
    res <- lift $ loadSubscriptions fn
    subs <- except res
    res' <- lift $ saveSubscriptions (snoc subs sub) fn
    except res'

type BoundarySubscription =
  { id :: String
  , email :: String
  , bbox :: { a :: LatLng, b :: LatLng }
  , tags :: Array String
  , frequency :: String
  , changeType :: String
  , confirmed :: Boolean
  }

fromBoundarySubscription :: BoundarySubscription -> Maybe Subscription
fromBoundarySubscription { id, email, bbox, tags, frequency, changeType, confirmed } = do
  let bbox' = bbox.a /\ bbox.b
  frequency' <- fromBoundaryFrequency frequency
  changeType' <- fromBoundaryChangeType changeType
  pure { id, email, bbox: bbox', tags, frequency: frequency', changeType: changeType', confirmed }

toBoundarySubscription :: Subscription -> BoundarySubscription
toBoundarySubscription { id, email, bbox, tags, frequency, changeType, confirmed } =
  { id
  , email
  , bbox: { a: fst bbox, b: snd bbox }
  , tags
  , frequency: toBoundaryFrequency frequency
  , changeType: toBoundaryChangeType changeType
  , confirmed
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


newtype MockRepo = MockRepo (Ref (Array Subscription))

makeMockRepo :: forall m. MonadEffect m => Array Subscription -> m MockRepo
makeMockRepo subs = liftEffect $ new subs >>= pure <<< MockRepo

mockRepoContent :: forall m. MonadEffect m => MockRepo -> m (Array Subscription)
mockRepoContent (MockRepo mockRepoRef) = liftEffect $ read mockRepoRef

instance Repo MockRepo where
  get id' (MockRepo repoRef) = do
    subs <- liftEffect $ read repoRef
    pure $ note (Other "Subscription not found") $ find (\{ id } -> id == id') subs
  create sub (MockRepo repoRef) = do
    liftEffect $ modify_ (flip snoc sub) repoRef
    pure $ Right unit

