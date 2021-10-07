module KvmMail.Server.Subscription.Api where

import ThisPrelude

import Control.Monad.Except (catchError, throwError)
import KvmMail.Server.Data (AppError(..))
import KvmMail.Server.Services.Mailer (class Mailer)
import KvmMail.Server.Services.SingletonRepo (class SingRepo, load)
import KvmMail.Server.Subscription.Entities (ConfirmationToken, mkBBox)
import KvmMail.Server.Subscription.Repo (class Repo)
import KvmMail.Server.Subscription.Repo as Repo
import KvmMail.Server.Subscription.Usecases as UC
import KvmMail.Shared.Boundary (Settings)

type SubscribePayload =
  { email :: String
  , title :: String
  , bbox :: { lat1 :: Number, lng1 :: Number, lat2 :: Number, lng2 :: Number }
  , tags :: Maybe (Array String)
  , lang :: Maybe String
  , frequency :: Maybe String
  , changeType :: Maybe String
  }

-- subscribe :: forall m. MonadAff m => SubscribePayload -> String -> m (Either AppError Unit)
-- subscribe pl apiBaseUrl = runExceptT do
--   let env = { subscription: { repo: Repo.defaultFileRepo, mailer: NMailer unit, apiBaseUrl } }
--   res <- flip runReaderT env $ subscribeFlow pl
--   void $ except res

subscribeFlow :: forall m repo mailer settingsRepo r.
  MonadAff m => Repo repo => Mailer mailer => SingRepo settingsRepo Settings =>
  MonadAsk { subscription :: { repo :: repo, mailer :: mailer, apiBaseUrl :: String | r }, settingsRepo :: settingsRepo | r } m =>
  SubscribePayload -> m (Either AppError ConfirmationToken)
subscribeFlow pl = runExceptT do
  { subscription: { repo, mailer, apiBaseUrl }, settingsRepo } <- ask
  let bbox = mkBBox { lat: pl.bbox.lat1, lng: pl.bbox.lng1 } { lat: pl.bbox.lat2, lng: pl.bbox.lng2 }
  let tags = fromMaybe [] pl.tags
  let slang = fromMaybe "en" pl.lang
  lang <- except $ note (InvalidInput $ "Unknown value for lang: " <> slang) $ Repo.fromBoundaryLang slang
  let sfreq = fromMaybe "hour" pl.frequency
  freq <- except $ note (InvalidInput $ "Unknown value for frequency: " <> sfreq) $ Repo.fromBoundaryFrequency sfreq
  let sct = fromMaybe "new" pl.changeType
  ct <- except $ note (InvalidInput $ "Unknown value for changeType: " <> sct) $ Repo.fromBoundaryChangeType sct
  id <- UC.subscribe pl.title pl.email lang bbox tags freq ct >>= except # withExceptT (OtherError <<< show)
  sub <- Repo.read id repo # withExceptT (OtherError <<< show)
  settings <- load settingsRepo
  token <- withExceptT (OtherError <<< show) $ UC.sendConfirmationMail sub settings.senderAddress apiBaseUrl mailer `catchError` \e -> do
    Repo.delete id repo # withExceptT UC.RepoError
    throwError e
  pure token
  -- throwError $ OtherError "subscribe failed ho ho"

confirm :: forall m repo r. MonadAff m => Repo repo =>
  MonadAsk { subscription :: { repo :: repo | r } | r } m =>
  String -> m (Either ConfirmError Unit)
confirm token = UC.confirmSubscription token >>= pure <<< lmap toConfirmErr
  where
  toConfirmErr (UC.RepoError Repo.NotFoundError) = CESubscriptionDoesNotExist
  toConfirmErr e = CEOtherError $ show e

data ConfirmError
  = CEOtherError String
  | CENoToken
  | CESubscriptionDoesNotExist

unsubscribe :: forall m repo r. MonadAff m => Repo repo =>
  MonadAsk { subscription :: { repo :: repo | r } | r } m =>
  String -> m (Either UnsubscribeError Unit)
-- unsubscribe token = runExceptT $ UC.unsubscribe token # withExceptT toUnsubscribeErr
unsubscribe token = UC.unsubscribe token >>= pure <<< lmap toUnsubscribeErr
  where
  toUnsubscribeErr (UC.RepoError Repo.NotFoundError) = UESubscriptionDoesNotExist
  toUnsubscribeErr e = UEOtherError $ show e

data UnsubscribeError
  = UEOtherError String
  | UENoToken
  | UESubscriptionDoesNotExist

