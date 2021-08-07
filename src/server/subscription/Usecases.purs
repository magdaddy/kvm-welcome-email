module WelcomeEmail.Server.Subscription.Usecases where

import Prelude

import Data.Either (Either(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import WelcomeEmail.Server.Mailer (class Mailer, MailerError)
import WelcomeEmail.Server.Subscription.Entities (BBox, ChangeType, EmailAddr, Frequency, Id, Tag, Subscription)
import WelcomeEmail.Server.Subscription.Repo (class Repo, RepoError, create)
import WelcomeEmail.Shared.Util (eTodo, genId)

subscribe :: forall m repo.
  MonadAff m => Repo repo =>
  EmailAddr -> BBox -> Array Tag -> Frequency -> ChangeType -> repo -> m (Either RepoError Id)
subscribe email bbox tags frequency changeType repo = do
  id <- genId
  let sub = { id, email, bbox, tags, frequency, changeType, confirmed: false }
  void $ liftAff $ create sub repo
  pure $ Right id


unsubscribe :: forall repo. Repo repo => Id -> repo -> Aff Unit
unsubscribe _ _ = eTodo

sendConfirmationMail :: forall m mailer.
  MonadAff m => Mailer mailer =>
  Subscription -> mailer -> m (Either MailerError Unit)
sendConfirmationMail _ _ = eTodo

confirmSubscription :: forall repo. Repo repo => repo -> Aff Unit
confirmSubscription _ = eTodo

-- sendNotification ::

-- checkRecentlyChanged ::


