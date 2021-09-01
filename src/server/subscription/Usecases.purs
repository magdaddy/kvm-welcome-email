module WelcomeEmail.Server.Subscription.Usecases where

import ThisPrelude

import Control.Monad.Except (runExceptT)
import Data.Array as A
import Data.Bifunctor (lmap)
import Data.Generic.Rep (class Generic)
import Data.JSDate (JSDate, now)
import Data.Show.Generic (genericShow)
import Data.Time.Duration (Minutes(..), convertDuration)
import Data.Traversable (for_, traverse_)
import Effect.Aff (delay, launchAff_)
import Foreign (readString)
import Foreign.Index (readProp)
import MagLibs.DateFns as DFN
import Nmailer (NMailer(..))
import WelcomeEmail.Server.Services.Jwt (Error) as Jwt
import WelcomeEmail.Server.Services.Mailer (Error, sendEmail) as Mailer
import WelcomeEmail.Server.Services.Mailer (class Mailer)
import WelcomeEmail.Server.Services.RecentlyChanged (class RecentlyChanged, defaultRecentlyChangedFiles)
import WelcomeEmail.Server.Services.RecentlyChanged as RecentlyChanged
import WelcomeEmail.Server.Settings (loadSettings)
import WelcomeEmail.Server.Subscription.EmailTemplates (confirmationMail, digestMail)
import WelcomeEmail.Server.Subscription.Entities (BBox, ChangeType(..), ConfirmationToken, EmailAddr, Frequency(..), Id, Lang, SubscriptionAndDigest, Tag, UnsubscribeToken, Subscription)
import WelcomeEmail.Server.Subscription.Repo (class Repo, defaultFileRepo)
import WelcomeEmail.Server.Subscription.Repo as Repo
import WelcomeEmail.Server.Util (isInBBox, jwtDecode, jwtSign, jwtVerifyS)
import WelcomeEmail.Shared.Boundary (EntryChange)
import WelcomeEmail.Shared.Util (genId16, logExceptConsole)


data SubscriptionError
  = OtherError String
  | JwtError Jwt.Error
  | RepoError Repo.Error
  | MailerError Mailer.Error
  | OfdbError RecentlyChanged.Error

derive instance Eq SubscriptionError
derive instance Generic SubscriptionError _
instance Show SubscriptionError where show = genericShow


subscribe :: forall m repo.
  MonadAff m => Repo repo =>
  String -> EmailAddr -> Lang -> BBox -> Array Tag -> Frequency -> ChangeType -> repo ->
  ExceptT Repo.Error m Id
subscribe title email lang bbox tags frequency changeType repo = do
  id <- genId16
  secret <- genId16
  now <- liftEffect now
  let sub = { id, title, email, lang, bbox, tags, frequency, changeType, confirmed: false, secret, lastSent: now, created: now }
  Repo.create sub repo
  pure id

unsubscribe :: forall m repo.
  MonadAff m => Repo repo =>
  UnsubscribeToken -> repo -> ExceptT SubscriptionError m Unit
unsubscribe token repo = do
  sub <- verifyToken token repo >>= except
  Repo.delete sub.id repo # withExceptT RepoError

sendConfirmationMail :: forall m mailer.
  MonadAff m => Mailer mailer =>
  Subscription -> String -> String -> mailer -> ExceptT SubscriptionError m ConfirmationToken
sendConfirmationMail sub from apiBaseUrl mailer = do
  let token = jwtSign {} sub.secret { subject: sub.id }
  let url = apiBaseUrl <> "/confirm-subscription?token=" <> token
  let { subject, body } = confirmationMail sub url
  let email = { from, to: [ sub.email ], subject, body }
  _ <- Mailer.sendEmail email mailer >>= withExceptT MailerError <<< except
  pure $ token

confirmSubscription :: forall m repo.
  MonadAff m => Repo repo =>
  ConfirmationToken -> repo -> ExceptT SubscriptionError m Unit
confirmSubscription token repo = do
  sub <- verifyToken token repo >>= except
  Repo.update (sub { confirmed = true }) repo # withExceptT RepoError

sendNotificationMail :: forall m mailer.
  MonadAff m => Mailer mailer =>
  Subscription -> Array EntryChange -> String -> String -> mailer -> ExceptT SubscriptionError m UnsubscribeToken
sendNotificationMail sub digest from apiBaseUrl mailer = do
  let token = jwtSign {} sub.secret { subject: sub.id }
  let url = apiBaseUrl <> "/unsubscribe?token=" <> token
  let { subject, body } = digestMail sub digest url
  let email = { from, to: [ sub.email ], subject, body }
  _ <- Mailer.sendEmail email mailer >>= withExceptT MailerError <<< except
  pure $ token

checkRecentlyChanged :: forall m rc repo.
  MonadAff m => RecentlyChanged rc => Repo repo =>
  JSDate -> rc -> repo -> ExceptT SubscriptionError m (Array SubscriptionAndDigest)
checkRecentlyChanged now rc repo = do
  entries <- RecentlyChanged.recentlyChanged rc # withExceptT OfdbError
  allSubs <- Repo.readAll repo # withExceptT RepoError
  -- let allSubs = filterOutOldUnconfirmed allSubs
  let subs = A.filter (subscriptionShouldSendNotification now) allSubs
  let snds = map (\sub -> { digest: getDigest sub entries, sub }) subs
  pure $ A.filter (\snd -> not A.null snd.digest) snds

runSubscriptionNotificationService :: forall m. MonadEffect m => String -> m Unit
runSubscriptionNotificationService apiBaseUrl = do
  let
    rc = defaultRecentlyChangedFiles
    repo = defaultFileRepo
    mailer = NMailer unit
    loop = do
      -- log "checking subscriptions"
      settings <- liftEffect $ loadSettings
      now <- liftEffect now
      logExceptConsole do
        entries <- RecentlyChanged.recentlyChanged rc # withExceptT OfdbError
        allSubs <- Repo.readAll repo # withExceptT RepoError
        for_ (A.filter (unconfirmedAndTooOld now) allSubs) \sub -> do
          Repo.delete sub.id repo # withExceptT RepoError
        -- snds <- checkRecentlyChanged now rc repo
        traverse_ (checkThenSendNotificationAndUpdateLastSent repo mailer now settings.senderAddress apiBaseUrl entries) allSubs
      delay $ convertDuration $ Minutes 7.0
      loop
  liftEffect $ launchAff_ loop

-- PRIVATE USECASES --

checkThenSendNotificationAndUpdateLastSent :: forall m repo mailer. MonadAff m => Repo repo => Mailer mailer =>
  repo -> mailer -> JSDate -> String -> String -> Array EntryChange -> Subscription -> ExceptT SubscriptionError m Unit
checkThenSendNotificationAndUpdateLastSent repo mailer now from apiBaseUrl entries sub = do
  let digest = getDigest sub entries
  when (not A.null digest && subscriptionShouldSendNotification now sub) do
    sendNotificationMail sub digest from apiBaseUrl mailer # runExceptT >>= case _ of
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


verifyToken :: forall m repo. -- jwt.
  MonadAff m => Repo repo => -- Jwt jwt =>
  String -> repo -> m (Either SubscriptionError Subscription)
verifyToken token repo = runExceptT do
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

