module Test.Subscription where

import Prelude

import Control.Monad.Except (except)
import Data.Array (head)
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Class (liftEffect)
import Effect.Console (log)
import Nmailer (NMailer(..))
import Test.Data (rapunzelMitteLinks, subMitteLinks, subMitteRechts, subWest, urbanharbourWest, weltenkuecheWest)
import Test.Spec (Spec, describe, describeOnly, it, itOnly)
import Test.Spec.Assertions (fail, shouldEqual, shouldReturn, shouldSatisfy)
import Test.Util (mkDate, resultShouldSatisfy, shouldNotThrow, shouldThrow)
import WelcomeEmail.Server.Services.CrudRepo (FileRepo(..))
import WelcomeEmail.Server.Services.Mailer (Error(..), MockMailer(..)) as Mailer
import WelcomeEmail.Server.Services.Ofdb as Ofdb
import WelcomeEmail.Server.Services.OfdbApi (OfdbApiRest(..), defaultRcQuery, getEntriesRecentlyChanged)
import WelcomeEmail.Server.Subscription.Api (SubscribePayload, subscribeFlow)
import WelcomeEmail.Server.Subscription.Entities (ChangeType(..), Frequency(..), Subscription)
import WelcomeEmail.Server.Subscription.Repo as Repo
import WelcomeEmail.Server.Subscription.Usecases as UC
import WelcomeEmail.Shared.Util (genId16)



exSubscription :: Subscription
exSubscription =
  { id: "abcd"
  , bbox: { lat: 44.5, lng: 2.2 } /\ { lat: 44.5, lng: 2.2 }
  , email: "la@lu.com"
  , tags: []
  , frequency: Day
  , changeType: NewEntries
  , confirmed: false
  , secret: "sdffds"
  , lastSent: mkDate 2021 7 12 9 15
  }

exSubPayload :: SubscribePayload
exSubPayload =
  { email: "la@lu.com"
  , bbox: { lat1: 44.5, lng1: 2.2, lat2: 44.5, lng2: 2.2 }
  , tags: Nothing
  , frequency: Just "day"
  , changeType: Just "new"
  }

subscriptionSpec :: Spec Unit
subscriptionSpec = do
  describeOnly "Subscription" do
    describe "Repo" do
      it "create works" do
        mockRepo <- Repo.mkMock []
        Repo.create exSubscription mockRepo # shouldNotThrow
        Repo.mockRepoContent mockRepo `shouldReturn` [exSubscription]
      it "get works" do
        mockRepo <- Repo.mkMock [exSubscription]
        (Repo.read exSubscription.id mockRepo # shouldNotThrow) `shouldReturn` exSubscription
    describe "FileRepo" do
      it "crud works" do
        fn <- genId16 >>= pure <<< ("data/tmp/filerepo_" <> _) <<< (_ <> ".json")
        let repo = FileRepo fn
        let sub = exSubscription
        Repo.create sub repo # shouldNotThrow
        (Repo.readAll repo # shouldNotThrow) `shouldReturn` [ sub ]
        (Repo.read sub.id repo # shouldNotThrow) `shouldReturn` sub
        Repo.update (sub { email = "nana@gugu.org" }) repo # shouldNotThrow
        (Repo.readAll repo # shouldNotThrow) `shouldReturn` [ sub { email = "nana@gugu.org" } ]
        Repo.delete sub.id repo # shouldNotThrow
        (Repo.readAll repo # shouldNotThrow) `shouldReturn` []
    describe "Flow" do
      -- it "subscribe works" do
      --   mockRepo <- mkMock []
      --   let s = exSubscription
      --   res <- subscribe s.email s.bbox s.tags s.frequency s.changeType mockRepo
      --   case res of
      --     Left _ -> fail ""
      --     Right id -> mockRepoContent mockRepo `shouldReturn` [s { id = id }]
      it "subscribeFlow works" do
        mockRepo <- Repo.mkMock []
        let mockMailer = Mailer.MockMailer $ Right unit
        let s = exSubPayload
        _ <- subscribeFlow s mockRepo mockMailer >>= except # shouldNotThrow
        pure unit
      it "subscribeFlow throws" do
        mockRepo <- Repo.mkMock []
        let mockMailer = Mailer.MockMailer $ Left $ Mailer.OtherError "Mail not send"
        let s = exSubPayload
        subscribeFlow s mockRepo mockMailer >>= except # shouldThrow
      it "sub + conf + unsub works" do
        mockRepo <- Repo.mkMock []
        let mockMailer = Mailer.MockMailer $ Right unit
        let s = exSubPayload
        confToken <- subscribeFlow s mockRepo mockMailer >>= except # shouldNotThrow
        UC.confirmSubscription confToken mockRepo # shouldNotThrow
        rc <- Repo.mockRepoContent mockRepo
        case head rc of
          Nothing -> fail "repo is empty"
          Just sub -> do
            sub.confirmed `shouldEqual` true
            unsubToken <- UC.sendNotificationMail sub [] mockMailer # shouldNotThrow
            UC.unsubscribe unsubToken mockRepo # shouldNotThrow
            Repo.mockRepoContent mockRepo `shouldReturn` []
      it "checkRecentlyChanged" do
        let rcEntries =
              [ { changed: mkDate 2021 8 13 10 0, entry: rapunzelMitteLinks }
              , { changed: mkDate 2021 8 13 10 0, entry: urbanharbourWest }
              , { changed: mkDate 2021 8 12 12 0, entry: weltenkuecheWest }
              ]
        let mockOfdb = Ofdb.Mock $ Right rcEntries
        mockRepo <- Repo.mkMock
          [ subMitteLinks
          , subWest { lastSent = mkDate 2021 8 12 11 0 }
          , subMitteRechts
          ]
        let now = mkDate 2021 8 13 12 0
        UC.subscriptionShouldSendNotification now subMitteLinks `shouldEqual` true
        UC.subscriptionShouldSendNotification now subMitteRechts `shouldEqual` true
        UC.subscriptionShouldSendNotification now subWest `shouldEqual` true
        snds <- UC.checkRecentlyChanged now mockOfdb mockRepo # shouldNotThrow
        traverse_ (liftEffect <<< log <<< show) snds
        let mailer = NMailer unit
        traverse_ (\{ sub, digest } -> UC.sendNotificationMail sub digest mailer # shouldNotThrow) snds
        pure unit
    describeOnly "OfdbApi" do
      it "ofdbapi" do
        let api = OfdbApiRest { baseUrl: "https://api.ofdb.io/v0" }
        -- let query = defaultRcQuery { since = Just $ mkDate 2021 8 13 12 0, withRatings = Just true }
        let query = defaultRcQuery { withRatings = Just true }
        entries <- getEntriesRecentlyChanged query api # shouldNotThrow
        liftEffect $ log $ show entries
        pure unit
