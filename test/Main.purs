module Test.Main where

import Prelude

import Control.Monad.Except (catchError, runExceptT)
import Data.Either (Either)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Math (e)
import Nmailer (NMailer(..))
import Test.Data (subMitteLinks)
import Test.Spec (Spec)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)
import Test.Subscription (subscriptionSpec)
import Test.Turf (turfSpec)
import WelcomeEmail.Server.Subscription.Usecases (SubscriptionError)
import WelcomeEmail.Server.Subscription.Usecases as UC

main :: Effect Unit
main = launchAff_ do
  liftEffect $ log "Started he he..."
  _ <- sendConfMail
  runSpec [consoleReporter] do
    allSpecs

allSpecs :: Spec Unit
allSpecs = do
  turfSpec
  subscriptionSpec


sendConfMail :: forall m. MonadAff m => m (Either SubscriptionError String)
sendConfMail = do
  runExceptT $ UC.sendConfirmationMail subMitteLinks (NMailer unit) `catchError` \e -> do
    (liftEffect <<< log <<< show) e
    pure "ola"
