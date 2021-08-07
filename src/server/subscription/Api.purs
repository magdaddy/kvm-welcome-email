module WelcomeEmail.Server.Subscription.Api where

import Prelude

import Control.Monad.Except (except, runExceptT, throwError)
import Data.Bifunctor (lmap)
import Data.Either (Either, note)
import Data.Maybe (Maybe, fromMaybe)
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff)
import WelcomeEmail.Server.Data (AppError(..))
import WelcomeEmail.Server.Subscription.Repo (class Repo, defaultFileRepo, fromBoundaryChangeType, fromBoundaryFrequency)
import WelcomeEmail.Server.Subscription.Usecases (subscribe) as UC

type SubscribePayload =
  { email :: String
  , bbox :: { lat1 :: Number, lng1 :: Number, lat2 :: Number, lng2 :: Number }
  , tags :: Maybe (Array String)
  , frequency :: Maybe String
  , changeType :: Maybe String
  }

subscribe :: forall m. MonadAff m => SubscribePayload -> m (Either AppError Unit)
subscribe pl = subscribeFlow pl defaultFileRepo

subscribeFlow :: forall m repo.
  MonadAff m => Repo repo =>
  SubscribePayload -> repo -> m (Either AppError Unit)
subscribeFlow pl repo = runExceptT do
  let bbox = { lat: pl.bbox.lat1, lng: pl.bbox.lng1 } /\ { lat: pl.bbox.lat2, lng: pl.bbox.lng2 }
  let tags = fromMaybe [] pl.tags
  let sfreq = fromMaybe "hour" pl.frequency
  freq <- except $ note (OtherError $ "Unknown value for frequency: " <> sfreq) $ fromBoundaryFrequency sfreq
  let sct = fromMaybe "new" pl.changeType
  ct <- except $ note (OtherError $ "Unknown value for changeType: " <> sct) $ fromBoundaryChangeType sct
  res <- UC.subscribe pl.email bbox tags freq ct repo
  _id <- except $ lmap (OtherError <<< show) res
  pure unit
  -- throwError $ OtherError "subscribe failed ho ho"

