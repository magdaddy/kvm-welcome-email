module KvmMail.Server.Services.RecentlyChanged where

import ThisPrelude

import Control.Monad.Except (catchError)
import Data.Array as A
import Data.Generic.Rep (class Generic)
import Data.JSDate (now)
import Data.Set as Set
import Data.Show.Generic (genericShow)
import Data.Time.Duration (Minutes(..), convertDuration)
import Data.Tuple.Nested ((/\))
import Effect.Aff (delay, launchAff_)
import KvmMail.Server.Services.OfdbApi (class OfdbApi, OfdbApiRest(..), defaultRcQuery, getEntriesRecentlyChanged)
import KvmMail.Server.Services.SingletonRepo (class SingletonRepo, SingletonFileRepo(..), loadA)
import KvmMail.Server.Services.SingletonRepo as SingletonRepo
import KvmMail.Shared.Boundary (EntryChange, EntryChangeA(..))
import KvmMail.Shared.Entry (Entry)
import KvmMail.Shared.Util (logExceptConsole)


data Error
  = OtherError String

derive instance Eq Error
derive instance Generic Error _
instance Show Error where show = genericShow

class RecentlyChanged rc where
  recentlyChanged :: forall m. MonadAff m => rc -> ExceptT Error m (Array EntryChange)


newtype RecentlyChangedFiles rcR = RecentlyChangedFiles { recentlyChangedRepo :: rcR }

instance RecentlyChanged (RecentlyChangedFiles SingletonFileRepo) where
  recentlyChanged (RecentlyChangedFiles { recentlyChangedRepo }) = do
    EntryChangeA eca <- loadA recentlyChangedRepo # withExceptT (OtherError <<< show)
    pure eca


updateFeed :: forall m ofdbApi rcR.
  MonadAff m => OfdbApi ofdbApi => SingletonRepo rcR EntryChangeA =>
  ofdbApi -> RecentlyChangedFiles rcR -> ExceptT Error m Unit
updateFeed ofdbApi (RecentlyChangedFiles { recentlyChangedRepo }) = do
  entries <- getEntriesRecentlyChanged defaultRcQuery { withRatings = Just true } ofdbApi >>= except # withExceptT (OtherError <<< show)
  repoEntries :: Array EntryChange <- (SingletonRepo.loadA recentlyChangedRepo >>= pure <<< unwrap) `catchError` (\_ -> pure []) # withExceptT (OtherError <<< show)
  now <- liftEffect now
  let dedupEs = dedupEntries entries repoEntries
  let (newRepoEntries :: Array EntryChange) = ((\entry -> { changed: now, entry }) <$> dedupEs) <> repoEntries
  SingletonRepo.saveA (wrap newRepoEntries) recentlyChangedRepo # withExceptT (OtherError <<< show)
  pure unit

dedupEntries :: Array Entry -> Array EntryChange -> Array Entry
dedupEntries newEntries repoEntries = A.filter notInRepo newEntries
  where
  notInRepo entry = not Set.member (entry.id /\ entry.version) idSet
  idSet = Set.fromFoldable ((\{ entry } -> entry.id /\ entry.version) <$> repoEntries)

-- defaultRecentlyChanged :: forall rc. RecentlyChanged rc => rc
-- defaultRecentlyChanged = defaultRecentlyChangedFiles

defaultRecentlyChangedFiles :: RecentlyChangedFiles SingletonFileRepo
defaultRecentlyChangedFiles = RecentlyChangedFiles { recentlyChangedRepo: SingletonFileRepo "data/recently-changed.json" }

runRecentlyChangedService :: forall m. MonadEffect m => m Unit
runRecentlyChangedService = liftEffect $ launchAff_ loop
  where
  rcRepo = defaultRecentlyChangedFiles
  ofdbApi = OfdbApiRest { baseUrl: "https://api.ofdb.io/v0" }
  loop = do
    logExceptConsole $ updateFeed ofdbApi rcRepo
    delay $ convertDuration $ Minutes 11.0
    loop


newtype Mock = Mock (Either Error (Array EntryChange))

instance RecentlyChanged Mock where
  recentlyChanged (Mock res) = except res


