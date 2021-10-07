module Test.Subscription.CheckRecentlyChanged where

import Test.Spec
import Test.Spec.Assertions
import ThisPrelude

import Data.Array as A
import Data.JSDate (JSDate)
import Data.Traversable (traverse)
import MagLibs.DateFns as DFN
import Test.Data as Data
import Test.QCTest (chooseJSDate, genEntryChangeForSub, genSubscription)
import Test.QuickCheck (randomSeed)
import Test.QuickCheck.Gen (Gen, evalGen, vectorOf)
import Test.Util (mkDate, shouldReturnRight)
import KvmMail.Server.Services.RecentlyChanged as RecentlyChanged
import KvmMail.Server.Subscription.Entities (ChangeType(..), Frequency(..), Subscription)
import KvmMail.Server.Subscription.Repo as Repo
import KvmMail.Server.Subscription.Usecases (subscriptionDueDate)
import KvmMail.Server.Subscription.Usecases as UC
import KvmMail.Shared.Util (repeatMA)

checkRecentlyChangedSpec :: Spec Unit
checkRecentlyChangedSpec = describe "Check recently changed" do
  it "checkRecentlyChanged" do
    let rcEntries =
          [ { changed: mkDate 2021 8 13 10 0, entry: Data.rapunzelMitteLinks }
          , { changed: mkDate 2021 8 13 10 0, entry: Data.urbanharbourWest }
          , { changed: mkDate 2021 8 12 12 0, entry: Data.weltenkuecheWest }
          ]
    let mockRc = RecentlyChanged.Mock $ Right rcEntries
    mockRepo <- Repo.mkMock
      [ Data.subMitteLinks
      , Data.subWest { lastSent = mkDate 2021 8 12 11 0 }
      , Data.subMitteRechts
      ]
    let now = mkDate 2021 8 13 12 0
    let env = { subscription: { repo: mockRepo } }
    UC.subscriptionShouldSendNotification now Data.subMitteLinks `shouldEqual` true
    UC.subscriptionShouldSendNotification now Data.subMitteRechts `shouldEqual` true
    UC.subscriptionShouldSendNotification now Data.subWest `shouldEqual` true
    _snds <- UC.checkRecentlyChanged now mockRc # flip runReaderT env # shouldReturnRight
    -- traverse_ (liftEffect <<< log <<< show) snds
    -- let mailer = NMailer unit
    -- traverse_ (\{ sub, digest } -> UC.sendNotificationMail sub digest mailer # shouldNotThrow) snds
    pure unit
  it "sends only to confirmed subscriptions" do
    repeatMA dR do
      -- generate genId >>= log
      now <- genNow
      -- subs <- generate $ genSubsNoIntersect 7
      subs <- generate $ vectorOf 20 genSubscription
        >>= pure <<< map _ { changeType = AllEntries }
        >>= traverse (genModifyLastSent $ chooseJSDate (DFN.add { years: -1 } now) (DFN.add { weeks: -2 } now))
      let subsConfirmed = A.partition (\s -> s.confirmed == true) subs
      -- log $ show no
      -- log $ show yes
      let genECsForSubs ss = map A.concat $ traverse (vectorOf 5 <<< genEntryChangeForSub (ago40min now)) ss
      rces <- generate $ genECsForSubs subs
      -- traverse_ (log <<< showEntryChange) rces
      mockRepo <- Repo.mkMock subs
      let mockRc = RecentlyChanged.Mock $ Right rces
      let env = { subscription: { repo: mockRepo } }
      snds <- UC.checkRecentlyChanged now mockRc # flip runReaderT env # shouldReturnRight
      let resultSubs = map (\snd -> snd.sub) snds
      resultSubs `shouldEqual` subsConfirmed.yes
  it "sends only when subscription is due" do
    repeatMA dR do
      -- generate genId >>= log
      now <- genNow
      subs <- generate $ vectorOf 20 genSubscription
        >>= pure <<< map _ { confirmed = true, changeType = AllEntries }
        >>= traverse (genModifyLastSent $ chooseJSDate (DFN.add { weeks: -1 } now) (DFN.add { weeks: 1 } now))
      let subsDue = A.partition (\s -> subscriptionDueDate s <= now) subs
      let genECsForSubs ss = map A.concat $ traverse (vectorOf 5 <<< genEntryChangeForSub (ago40min now)) ss
      rces <- generate $ genECsForSubs subs
      mockRepo <- Repo.mkMock subs
      let mockRc = RecentlyChanged.Mock $ Right rces
      let env = { subscription: { repo: mockRepo } }
      snds <- UC.checkRecentlyChanged now mockRc # flip runReaderT env # shouldReturnRight
      let resultSubs = map (\snd -> snd.sub) snds
      resultSubs `shouldEqual` subsDue.yes


dR = 75 :: Int

generate :: forall a m. MonadEffect m => Gen a -> m a
generate gen = do
  seed <- liftEffect $ randomSeed
  pure $ evalGen gen { newSeed: seed, size: 10 }

genNow :: forall m. MonadEffect m => m JSDate
genNow = generate $ chooseJSDate (mkDate 2021 8 13 12 0) (mkDate 2021 7 1 12 0)

genModifyLastSent :: Gen JSDate -> Subscription -> Gen Subscription
genModifyLastSent genJsDate sub = do
  lastSent <- genJsDate
  pure $ sub { lastSent = lastSent }

ago40min :: JSDate -> Gen JSDate
ago40min now = chooseJSDate now (DFN.add { minutes: -40 } now)

partitionFrequency :: Array Subscription -> { hour :: Array Subscription, day :: Array Subscription, week :: Array Subscription }
partitionFrequency subs = { hour, day, week }
  where
  h = A.partition (\s -> s.frequency == Hour) subs
  d = A.partition (\s -> s.frequency == Day) h.no
  hour = h.yes
  day = d.yes
  week = d.no
