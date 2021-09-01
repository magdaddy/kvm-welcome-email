module WelcomeEmail.Server.Subscription.Api where

import ThisPrelude

import Control.Monad.Except (catchError, runExceptT, throwError)
import Data.Either (note)
import Data.Maybe (fromMaybe)
import Nmailer (NMailer(..))
import WelcomeEmail.Server.Data (AppError(..))
import WelcomeEmail.Server.Services.Mailer (class Mailer)
import WelcomeEmail.Server.Subscription.Entities (ConfirmationToken, mkBBox)
import WelcomeEmail.Server.Subscription.Repo (class Repo)
import WelcomeEmail.Server.Subscription.Repo as Repo
import WelcomeEmail.Server.Subscription.Usecases as UC

type SubscribePayload =
  { email :: String
  , title :: String
  , bbox :: { lat1 :: Number, lng1 :: Number, lat2 :: Number, lng2 :: Number }
  , tags :: Maybe (Array String)
  , lang :: Maybe String
  , frequency :: Maybe String
  , changeType :: Maybe String
  }

subscribe :: forall m. MonadAff m => SubscribePayload -> String -> m (Either AppError Unit)
subscribe pl apiBaseUrl = runExceptT do
  let mailer = NMailer unit
  res <- subscribeFlow pl apiBaseUrl Repo.defaultFileRepo mailer
  void $ except res

subscribeFlow :: forall m repo mailer.
  MonadAff m => Repo repo => Mailer mailer =>
  SubscribePayload -> String -> repo -> mailer -> m (Either AppError ConfirmationToken)
subscribeFlow pl apiBaseUrl repo mailer = runExceptT do
  let bbox = mkBBox { lat: pl.bbox.lat1, lng: pl.bbox.lng1 } { lat: pl.bbox.lat2, lng: pl.bbox.lng2 }
  let tags = fromMaybe [] pl.tags
  let slang = fromMaybe "en" pl.lang
  lang <- except $ note (InvalidInput $ "Unknown value for lang: " <> slang) $ Repo.fromBoundaryLang slang
  let sfreq = fromMaybe "hour" pl.frequency
  freq <- except $ note (InvalidInput $ "Unknown value for frequency: " <> sfreq) $ Repo.fromBoundaryFrequency sfreq
  let sct = fromMaybe "new" pl.changeType
  ct <- except $ note (InvalidInput $ "Unknown value for changeType: " <> sct) $ Repo.fromBoundaryChangeType sct
  id <- UC.subscribe pl.title pl.email lang bbox tags freq ct repo # withExceptT (OtherError <<< show)
  sub <- Repo.read id repo # withExceptT (OtherError <<< show)
  token <- withExceptT (OtherError <<< show) $ UC.sendConfirmationMail sub apiBaseUrl mailer `catchError` \e -> do
    Repo.delete id repo # withExceptT UC.RepoError
    throwError e
  pure token
  -- throwError $ OtherError "subscribe failed ho ho"

confirm :: forall m. MonadAff m => String -> ExceptT ConfirmError m Unit
confirm token = UC.confirmSubscription token Repo.defaultFileRepo # withExceptT toConfirmErr
  where
  toConfirmErr (UC.RepoError Repo.NotFoundError) = CESubscriptionDoesNotExist
  toConfirmErr e = CEOtherError $ show e

data ConfirmError
  = CEOtherError String
  | CENoToken
  | CESubscriptionDoesNotExist

unsubscribe :: forall m. MonadAff m => String -> ExceptT UnsubscribeError m Unit
unsubscribe token = UC.unsubscribe token Repo.defaultFileRepo # withExceptT toUnsubscribeErr
  where
  toUnsubscribeErr (UC.RepoError Repo.NotFoundError) = UESubscriptionDoesNotExist
  toUnsubscribeErr e = UEOtherError $ show e

data UnsubscribeError
  = UEOtherError String
  | UENoToken
  | UESubscriptionDoesNotExist

