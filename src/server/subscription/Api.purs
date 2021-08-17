module WelcomeEmail.Server.Subscription.Api where

import Prelude

import Control.Monad.Except (ExceptT, except, runExceptT, withExceptT)
import Data.Either (Either(..), note)
import Data.Maybe (Maybe, fromMaybe)
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff)
import Nmailer (NMailer(..))
import WelcomeEmail.Server.Data (AppError(..))
import WelcomeEmail.Server.Services.Mailer (class Mailer, MockMailer(..))
import WelcomeEmail.Server.Subscription.Entities (ConfirmationToken)
import WelcomeEmail.Server.Subscription.Repo (class Repo)
import WelcomeEmail.Server.Subscription.Repo as Repo
import WelcomeEmail.Server.Subscription.Usecases (confirmSubscription)
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
  -- let mailer = MockMailer $ Right unit
  let mailer = NMailer unit
  res <- subscribeFlow pl Repo.defaultFileRepo mailer
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
  freq <- except $ note (OtherError $ "Unknown value for frequency: " <> sfreq) $ Repo.fromBoundaryFrequency sfreq
  let sct = fromMaybe "new" pl.changeType
  ct <- except $ note (OtherError $ "Unknown value for changeType: " <> sct) $ Repo.fromBoundaryChangeType sct
  id <- UC.subscribe pl.email bbox tags freq ct repo # withExceptT (OtherError <<< show)
  sub <- Repo.read id repo # withExceptT (OtherError <<< show)
  token <- UC.sendConfirmationMail sub mailer # withExceptT (OtherError <<< show)
  pure token
  -- throwError $ OtherError "subscribe failed ho ho"

confirm :: forall m. MonadAff m => String -> ExceptT AppError m Unit
confirm token = confirmSubscription token Repo.defaultFileRepo # withExceptT (OtherError <<< show)
