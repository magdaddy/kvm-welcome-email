module WelcomeEmail.Server.Subscription.Usecases where

import Prelude

import Control.Monad.Except (ExceptT, except, runExceptT, withExceptT)
import Data.Array as A
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.JSDate (JSDate, fromTime)
import Data.Show.Generic (genericShow)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Foreign (readString)
import Foreign.Index (readProp)
import MagLibs.DateFns as DFN
import WelcomeEmail.Server.Services.Jwt (Error) as Jwt
import WelcomeEmail.Server.Services.Jwt (class Jwt)
import WelcomeEmail.Server.Services.Mailer (Error, sendEmail) as Mailer
import WelcomeEmail.Server.Services.Mailer (class Mailer)
import WelcomeEmail.Server.Services.Ofdb (Error, recentlyChanged) as Ofdb
import WelcomeEmail.Server.Services.Ofdb (class Ofdb, EntryChange)
import WelcomeEmail.Server.Subscription.EmailTemplates (confirmationMail, placeCreatedMail)
import WelcomeEmail.Server.Subscription.Entities (BBox, ChangeType, ConfirmationToken, EmailAddr, Frequency(..), Id, Subscription, SubscriptionAndDigest, Tag, UnsubscribeToken)
import WelcomeEmail.Server.Subscription.Repo (class Repo)
import WelcomeEmail.Server.Subscription.Repo as Repo
import WelcomeEmail.Server.Util (isInBBox, jwtDecode, jwtSign, jwtVerifyS)
import WelcomeEmail.Shared.Util (genId16)


data SubscriptionError
  = OtherError String
  | JwtError Jwt.Error
  | RepoError Repo.Error
  | MailerError Mailer.Error
  | OfdbError Ofdb.Error

derive instance Eq SubscriptionError
derive instance Generic SubscriptionError _
instance Show SubscriptionError where show = genericShow


subscribe :: forall m repo.
  MonadAff m => Repo repo =>
  EmailAddr -> BBox -> Array Tag -> Frequency -> ChangeType -> repo -> ExceptT Repo.Error m Id
subscribe email bbox tags frequency changeType repo = do
  id <- genId16
  secret <- genId16
  let sub = { id, email, bbox, tags, frequency, changeType, confirmed: false, secret, lastSent: fromTime 0.0 }
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
  Subscription -> mailer -> ExceptT SubscriptionError m ConfirmationToken
sendConfirmationMail sub mailer = do
  let token = jwtSign {} sub.secret { subject: sub.id }
  let url = "http://localhost:4001/api/confirmSubscription?token=" <> token
  let { subject, body } = confirmationMail url
  let email = { from: "from", to: [ sub.email ], subject, body }
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
  Subscription -> Array EntryChange -> mailer -> ExceptT SubscriptionError m UnsubscribeToken
sendNotificationMail sub _digest mailer = do
  let token = jwtSign {} sub.secret { subject: sub.id }
  let url = "http://localhost:4001/api/unsubscribe?token=" <> token
  let { subject, body } = placeCreatedMail url
  let email = { from: "from", to: [ sub.email ], subject, body }
  _ <- Mailer.sendEmail email mailer >>= withExceptT MailerError <<< except
  pure $ token

checkRecentlyChanged :: forall m ofdb repo.
  MonadAff m => Ofdb ofdb => Repo repo =>
  JSDate -> ofdb -> repo -> ExceptT SubscriptionError m (Array SubscriptionAndDigest)
checkRecentlyChanged now ofdb repo = do
  entries <- Ofdb.recentlyChanged ofdb # withExceptT OfdbError
  allSubs <- Repo.readAll repo # withExceptT RepoError
  let subs = A.filter (subscriptionShouldSendNotification now) allSubs
  let snds = map (\sub -> { digest: getDigest sub entries, sub }) subs
  pure $ A.filter (\snd -> not A.null snd.digest) snds


-- PRIVATE USECASES --

getDigest :: Subscription -> Array EntryChange -> Array EntryChange
getDigest sub entries = A.filter (\ec -> ec.changed > sub.lastSent && isInBBox sub.bbox ec.entry) entries

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


subscriptionShouldSendNotification :: JSDate -> Subscription -> Boolean
subscriptionShouldSendNotification now sub = sub.confirmed && (subscriptionDueDate sub) < now

subscriptionDueDate :: Subscription -> JSDate
subscriptionDueDate sub = case sub.frequency of
  Hour -> DFN.add { hours: 1 } sub.lastSent
  Day -> DFN.add { days: 1 } sub.lastSent
  Week -> DFN.add { weeks: 1 } sub.lastSent

