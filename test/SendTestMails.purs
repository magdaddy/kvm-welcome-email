module Test.SendTestMails where

import ThisPrelude

import Data.Array.NonEmpty (toArray)
import Effect.Aff (launchAff_)
import Nmailer (NMailer(..))
import Test.QCTest (chooseJSDate, genEntry, genSubscription)
import Test.QuickCheck.Gen (arrayOf1)
import Test.Spec (it)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)
import Test.Subscription.CheckRecentlyChanged (generate)
import Test.Util (mkDate, shouldNotThrow, shouldReturnRight)
import KvmMail.Server.Subscription.Entities (Lang(..))
import KvmMail.Server.Subscription.Usecases as UC

apiBaseUrl = "http://localhost:4001/api" :: String

main :: Effect Unit
main = launchAff_ do
  runSpec [consoleReporter] do
    it "send confirmation mail english" do
      sub <- generate genSubscription >>= pure <<< _ { lang = EN }
      void $ UC.sendConfirmationMail sub "from" apiBaseUrl (NMailer unit) # shouldNotThrow
    it "send confirmation mail german" do
      sub <- generate genSubscription >>= pure <<< _ { lang = DE }
      void $ UC.sendConfirmationMail sub "from" apiBaseUrl (NMailer unit) # shouldNotThrow
    it "send notification mail english, german" do
      let env = { subscription: { mailer: NMailer unit, apiBaseUrl } }
      let
        genEc = do
          ch <- chooseJSDate (mkDate 2017 1 1 0 0) (mkDate 2021 1 1 0 0)
          e <- genEntry
          pure {changed: ch, entry: e}
      ecs <- generate $ arrayOf1 genEc
      subEn <- generate genSubscription >>= pure <<< _ { lang = EN }
      void $ UC.sendNotificationMail subEn (toArray ecs) "from" # flip runReaderT env # shouldReturnRight
      subDe <- generate genSubscription >>= pure <<< _ { lang = DE }
      void $ UC.sendNotificationMail subDe (toArray ecs) "from" # flip runReaderT env # shouldReturnRight

