module KvmMail.Server.Subscription.Usecases where

import ThisPrelude

import Data.Array as A
import Data.Generic.Rep (class Generic)
import Data.JSDate (JSDate, now)
import Data.Show.Generic (genericShow)
import Data.Time.Duration (Minutes(..), convertDuration)
import Data.Traversable (for_, traverse_)
import Effect.Aff (delay, launchAff_)
import Foreign (readString)
import Foreign.Index (readProp)
import MagLibs.DateFns as DFN
import KvmMail.Server.Services.Jwt (Error) as Jwt
import KvmMail.Server.Services.Mailer (Error, sendEmail) as Mailer
import KvmMail.Server.Services.Mailer (class Mailer)
import KvmMail.Server.Services.RecentlyChanged (class RecentlyChanged, defaultRecentlyChangedFiles)
import KvmMail.Server.Services.RecentlyChanged as RecentlyChanged
import KvmMail.Server.Services.SingletonRepo (class SingRepo, load)
import KvmMail.Server.Subscription.EmailTemplates (confirmationMail, digestMail)
import KvmMail.Server.Subscription.Entities (BBox, ChangeType(..), ConfirmationToken, EmailAddr, Frequency(..), Id, Lang, SubscriptionAndDigest, Tag, UnsubscribeToken, Subscription)
import KvmMail.Server.Subscription.Repo (class Repo)
import KvmMail.Server.Subscription.Repo as Repo
import KvmMail.Server.Util (isInBBox, jwtDecode, jwtSign, jwtVerifyS)
import KvmMail.Shared.Boundary (EntryChange, Settings)
import KvmMail.Shared.Util (genId16, logExceptConsole)


data SubscriptionError
  = OtherError String
  | JwtError Jwt.Error
  | RepoError Repo.Error
  | MailerError Mailer.Error
  | OfdbError RecentlyChanged.Error

derive instance Eq SubscriptionError
derive instance Generic SubscriptionError _
instance Show SubscriptionError where show = genericShow


subscribe :: forall m repo r.
  MonadAff m => Repo repo =>
  MonadAsk { subscription :: { repo :: repo | r } | r } m =>
  String -> EmailAddr -> Lang -> BBox -> Array Tag -> Frequency -> ChangeType ->
  -- ExceptT Repo.Error m Id
  m (Either Repo.Error Id)
subscribe title email lang bbox tags frequency changeType = runExceptT do
  { repo } <- asks _.subscription
  id <- genId16
  secret <- genId16
  now <- liftEffect now
  let sub = { id, title, email, lang, bbox, tags, frequency, changeType, confirmed: false, secret, lastSent: now, created: now }
  Repo.create sub repo
  pure id

unsubscribe :: forall m repo r.
  MonadAff m => Repo repo =>
  MonadAsk { subscription :: { repo :: repo | r } | r } m =>
  UnsubscribeToken -> m (Either SubscriptionError Unit)
unsubscribe token = runExceptT do
  { repo } <- asks _.subscription
  sub <- verifyToken token >>= except
  Repo.delete sub.id repo # withExceptT RepoError

sendConfirmationMail :: forall m mailer.
  MonadAff m => Mailer mailer =>
  -- Subscription -> String -> String -> mailer -> m (Either SubscriptionError ConfirmationToken)
  Subscription -> String -> String -> mailer -> ExceptT SubscriptionError m ConfirmationToken
sendConfirmationMail sub from apiBaseUrl mailer = do
  let token = jwtSign {} sub.secret { subject: sub.id }
  let url = apiBaseUrl <> "/confirm-subscription?token=" <> token
  let { subject, body } = confirmationMail sub url
  let email = { from, to: [ sub.email ], subject, body }
  _ <- Mailer.sendEmail email mailer >>= withExceptT MailerError <<< except
  pure $ token

confirmSubscription :: forall m repo r.
  MonadAff m => Repo repo =>
  MonadAsk { subscription :: { repo :: repo | r } | r } m =>
  -- ConfirmationToken -> repo -> ExceptT SubscriptionError m Unit
  ConfirmationToken -> m (Either SubscriptionError Unit)
confirmSubscription token = runExceptT do
  { repo } <- asks _.subscription
  sub <- verifyToken token >>= except
  Repo.update (sub { confirmed = true }) repo # withExceptT RepoError

sendNotificationMail :: forall m mailer r.
  MonadAff m => Mailer mailer =>
  MonadAsk { subscription :: { mailer :: mailer, apiBaseUrl :: String | r } | r } m =>
  -- Subscription -> Array EntryChange -> String -> String -> mailer -> ExceptT SubscriptionError m UnsubscribeToken
  Subscription -> Array EntryChange -> String -> m (Either SubscriptionError UnsubscribeToken)
sendNotificationMail sub digest from = runExceptT do
  { mailer, apiBaseUrl } <- asks _.subscription
  let token = jwtSign {} sub.secret { subject: sub.id }
  let url = apiBaseUrl <> "/unsubscribe?token=" <> token
  let { subject, body } = digestMail sub digest url
  let email = { from, to: [ sub.email ], subject, body }
  _ <- Mailer.sendEmail email mailer >>= withExceptT MailerError <<< except
  pure $ token

checkRecentlyChanged :: forall m rc repo r.
  MonadAff m => RecentlyChanged rc => Repo repo =>
  MonadAsk { subscription :: { repo :: repo | r } | r } m =>
  -- JSDate -> rc -> repo -> ExceptT SubscriptionError m (Array SubscriptionAndDigest)
  JSDate -> rc -> m (Either SubscriptionError (Array SubscriptionAndDigest))
checkRecentlyChanged now rc = runExceptT do
  { repo } <- asks _.subscription
  entries <- RecentlyChanged.recentlyChanged rc # withExceptT OfdbError
  allSubs <- Repo.readAll repo # withExceptT RepoError
  -- let allSubs = filterOutOldUnconfirmed allSubs
  let subs = A.filter (subscriptionShouldSendNotification now) allSubs
  let snds = map (\sub -> { digest: getDigest sub entries, sub }) subs
  pure $ A.filter (\snd -> not A.null snd.digest) snds

runSubscriptionNotificationService :: forall m repo mailer settingsRepo r.
  MonadEffect m => Repo repo => Mailer mailer => SingRepo settingsRepo Settings =>
  MonadAsk { subscription :: { repo :: repo, mailer :: mailer, apiBaseUrl :: String | r }, settingsRepo :: settingsRepo | r } m =>
  m Unit
runSubscriptionNotificationService = do
  env <- ask
  let
    repo = env.subscription.repo
    rc = defaultRecentlyChangedFiles
    loop = do
      -- log "checking subscriptions"
      settings <- load env.settingsRepo
      now <- liftEffect now
      flip runReaderT env $ logExceptConsole do
        entries <- RecentlyChanged.recentlyChanged rc # withExceptT OfdbError
        allSubs <- Repo.readAll repo # withExceptT RepoError
        for_ (A.filter (unconfirmedAndTooOld now) allSubs) \sub -> do
          Repo.delete sub.id repo # withExceptT RepoError
        -- snds <- checkRecentlyChanged now rc repo
        traverse_ (checkThenSendNotificationAndUpdateLastSent now settings.senderAddress entries) allSubs
      delay $ convertDuration $ Minutes 7.0
      loop
  liftEffect $ launchAff_ loop

-- PRIVATE USECASES --

checkThenSendNotificationAndUpdateLastSent :: forall m repo mailer r. MonadAff m => Repo repo => Mailer mailer =>
  MonadAsk { subscription :: { repo :: repo, mailer :: mailer, apiBaseUrl :: String | r } | r } m =>
  -- repo -> mailer -> JSDate -> String -> String -> Array EntryChange -> Subscription -> ExceptT SubscriptionError m Unit
  JSDate -> String -> Array EntryChange -> Subscription -> m (Either SubscriptionError Unit)
checkThenSendNotificationAndUpdateLastSent now from entries sub = runExceptT do
  { repo } <- asks _.subscription
  let digest = getDigest sub entries
  when (not A.null digest && subscriptionShouldSendNotification now sub) do
    sendNotificationMail sub digest from >>= case _ of
      Left _ -> pure unit
      Right _ -> Repo.update sub { lastSent = now } repo # withExceptT RepoError

getDigest :: Subscription -> Array EntryChange -> Array EntryChange
getDigest sub entries = A.filter filterFunc entries
  where
  filterFunc ec = changedSinceLastSent && inBBox && tagsMatch && changeTypeMatches
    where
    changedSinceLastSent = ec.changed > sub.lastSent
    inBBox = isInBBox sub.bbox ec.entry
    tagsMatch = case sub.tags of
      [] -> true
      tags -> not A.null $ A.intersect tags ec.entry.tags
    changeTypeMatches = case sub.changeType of
      NewEntries -> ec.entry.version == 0
      AllEntries -> true


verifyToken :: forall m repo r. -- jwt.
  MonadAff m => Repo repo => -- Jwt jwt =>
  MonadAsk { subscription :: { repo :: repo | r } | r } m =>
  String -> m (Either SubscriptionError Subscription)
verifyToken token = runExceptT do
  { repo } <- asks _.subscription
  let for = jwtDecode token
  res <- runExceptT do
    sub <- readProp "sub" for
    readString sub
  id <- except $ lmap (OtherError <<< show) res
  sub <- Repo.read id repo # withExceptT RepoError
  _ <- jwtVerifyS token sub.secret # liftEffect >>= withExceptT OtherError <<< except
  pure sub

unconfirmedAndTooOld :: JSDate -> Subscription -> Boolean
unconfirmedAndTooOld now sub = unconfirmed && tooOld
  where
  unconfirmed = sub.confirmed == false
  tooOld = DFN.add { weeks: 2 } sub.created < now

subscriptionShouldSendNotification :: JSDate -> Subscription -> Boolean
subscriptionShouldSendNotification now sub = sub.confirmed && (subscriptionDueDate sub) < now

subscriptionDueDate :: Subscription -> JSDate
subscriptionDueDate sub = case sub.frequency of
  Hour -> DFN.add { hours: 1 } sub.lastSent
  Day -> DFN.add { days: 1 } sub.lastSent
  Week -> DFN.add { weeks: 1 } sub.lastSent

