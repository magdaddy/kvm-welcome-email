module WelcomeEmail.Server.Subscription.Api where

import Prelude

import Control.Monad.Except (except, runExceptT, withExceptT)
import Data.Either (Either(..), note)
import Data.Maybe (Maybe, fromMaybe)
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff)
import WelcomeEmail.Server.Data (AppError(..))
import WelcomeEmail.Server.Services.Mailer (class Mailer, MockMailer(..))
import WelcomeEmail.Server.Subscription.Entities (ConfirmationToken)
import WelcomeEmail.Server.Subscription.Repo (class Repo, defaultFileRepo, fromBoundaryChangeType, fromBoundaryFrequency, get)
import WelcomeEmail.Server.Subscription.Usecases as UC

type SubscribePayload =
  { email :: String
  , bbox :: { lat1 :: Number, lng1 :: Number, lat2 :: Number, lng2 :: Number }
  , tags :: Maybe (Array String)
  , frequency :: Maybe String
  , changeType :: Maybe String
  }

subscribe :: forall m. MonadAff m => SubscribePayload -> m (Either AppError Unit)
subscribe pl = runExceptT do
  let mailer = MockMailer $ Right unit
  res <- subscribeFlow pl defaultFileRepo mailer
  void $ except res

subscribeFlow :: forall m repo mailer.
  MonadAff m => Repo repo => Mailer mailer =>
  SubscribePayload -> repo ->
  mailer ->
  m (Either AppError ConfirmationToken)
subscribeFlow pl repo mailer = runExceptT do
  let bbox = { lat: pl.bbox.lat1, lng: pl.bbox.lng1 } /\ { lat: pl.bbox.lat2, lng: pl.bbox.lng2 }
  let tags = fromMaybe [] pl.tags
  let sfreq = fromMaybe "hour" pl.frequency
  freq <- except $ note (OtherError $ "Unknown value for frequency: " <> sfreq) $ fromBoundaryFrequency sfreq
  let sct = fromMaybe "new" pl.changeType
  ct <- except $ note (OtherError $ "Unknown value for changeType: " <> sct) $ fromBoundaryChangeType sct
  id <- UC.subscribe pl.email bbox tags freq ct repo >>= withExceptT (OtherError <<< show) <<< except
  sub <- get id repo >>= withExceptT (OtherError <<< show) <<< except
  token <- UC.sendConfirmationMail sub mailer # withExceptT (OtherError <<< show)
  pure token
  -- throwError $ OtherError "subscribe failed ho ho"

