module Test.Subscription where

import Test.Spec
import Test.Spec.Assertions
import ThisPrelude

import Data.Argonaut (jsonParser)
import Data.Array as A
import Data.Codec.Argonaut (printJsonDecodeError)
import Effect.Unsafe (unsafePerformEffect)
import Test.Subscription.CheckRecentlyChanged (checkRecentlyChangedSpec)
import Test.Util (mkDate, shouldNotThrow, shouldReturnLeft, shouldReturnRight)
import KvmMail.Server.Services.Mailer (Error(..), MockMailer(..)) as Mailer
import KvmMail.Server.Services.OfdbApi (OfdbApiRest(..), defaultRcQuery, getEntriesRecentlyChanged)
import KvmMail.Server.Services.RecentlyChanged (RecentlyChangedFiles(..), updateFeed)
import KvmMail.Server.Services.SingletonRepo (SingMemRoRepo(..), SingletonFileRepo(..))
import KvmMail.Server.Subscription.Api (SubscribePayload, subscribeFlow)
import KvmMail.Server.Subscription.Entities (ChangeType(..), Frequency(..), Lang(..), Subscription, mkBBox)
import KvmMail.Server.Subscription.Repo as Repo
import KvmMail.Server.Subscription.Usecases as UC
import KvmMail.Shared.Boundary (Settings)
import KvmMail.Shared.JsonCodecs (jsonDecode, settingsCdc)
import KvmMail.Shared.Util (throwLeft)

apiBaseUrl = "http://localhost:4001/api" :: String

settingsRepo :: SingMemRoRepo Settings
settingsRepo = SingMemRoRepo $ unsafePerformEffect $ throwLeft $ lmap printJsonDecodeError $
  jsonDecode settingsCdc $ unsafePerformEffect $ throwLeft $ jsonParser """
{
  "senderAddress": "magnus.herold@gmail.com",
  "nodeMailer": {
    "secure": false,
    "port": 1025,
    "host": "localhost",
    "auth": { "user": "", "pass": "" }
  },
  "defaultEntry": {
    "version": 33,
    "title": "Slowtec GmbH",
    "tags": [],
    "ratings": [],
    "lng": 120.7,
    "license": "",
    "lat": 37.2,
    "id": "4c20979fe0754e74875afa4308d73ce7",
    "description": "",
    "custom": [],
    "created": 1234,
    "country": "the country",
    "categories": []
  }
}"""

exSubscription :: Subscription
exSubscription =
  { id: "abcd"
  , title: "Title abcd"
  , bbox: mkBBox { lat: 44.5, lng: 2.2 } { lat: 44.5, lng: 2.2 }
  , email: "la@lu.com"
  , lang: DE
  , tags: []
  , frequency: Day
  , changeType: NewEntries
  , confirmed: false
  , secret: "sdffds"
  , lastSent: mkDate 2021 7 12 9 15
  , created: mkDate 2020 7 12 9 35
  }

exSubPayload :: SubscribePayload
exSubPayload =
  { email: "la@lu.com"
  , lang: Nothing
  , title: "Subscription Title"
  , bbox: { lat1: 44.5, lng1: 2.2, lat2: 44.5, lng2: 2.2 }
  , tags: Nothing
  , frequency: Just "day"
  , changeType: Just "new"
  }

subscriptionSpec :: Spec Unit
subscriptionSpec = do
  describeOnly "Subscription" do
    checkRecentlyChangedSpec
    describe "Repo" do
      it "create works" do
        mockRepo <- Repo.mkMock []
        Repo.create exSubscription mockRepo # shouldNotThrow
        Repo.mockRepoContent mockRepo `shouldReturn` [exSubscription]
      it "get works" do
        mockRepo <- Repo.mkMock [exSubscription]
        (Repo.read exSubscription.id mockRepo # shouldNotThrow) `shouldReturn` exSubscription
    -- describe "FileRepo" do
      -- it "crud works" do
      --   fn <- genId16 >>= pure <<< ("data/tmp/filerepo_" <> _) <<< (_ <> ".json")
      --   let repo = FileRepo fn
      --   let sub = exSubscription
      --   Repo.create sub repo # shouldNotThrow
      --   (Repo.readAll repo # shouldNotThrow) `shouldReturn` [ sub ]
      --   (Repo.read sub.id repo # shouldNotThrow) `shouldReturn` sub
      --   Repo.update (sub { email = "nana@gugu.org" }) repo # shouldNotThrow
      --   (Repo.readAll repo # shouldNotThrow) `shouldReturn` [ sub { email = "nana@gugu.org" } ]
      --   Repo.delete sub.id repo # shouldNotThrow
      --   (Repo.readAll repo # shouldNotThrow) `shouldReturn` []
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
        let env = { subscription: { repo: mockRepo, mailer: mockMailer, apiBaseUrl }, settingsRepo }
        let s = exSubPayload
        _confToken <- subscribeFlow s # flip runReaderT env # shouldReturnRight
        pure unit
      it "subscribeFlow throws" do
        mockRepo <- Repo.mkMock []
        let mockMailer = Mailer.MockMailer $ Left $ Mailer.OtherError "Mail not send"
        let env = { subscription: { repo: mockRepo, mailer: mockMailer, apiBaseUrl }, settingsRepo  }
        let s = exSubPayload
        subscribeFlow s # flip runReaderT env # shouldReturnLeft
        pure unit
      it "sub + conf + unsub works" do
        mockRepo <- Repo.mkMock []
        let mockMailer = Mailer.MockMailer $ Right unit
        let env = { subscription: { repo: mockRepo, mailer: mockMailer, apiBaseUrl }, settingsRepo  }
        let s = exSubPayload
        confToken <- subscribeFlow s # flip runReaderT env # shouldReturnRight
        UC.confirmSubscription confToken # flip runReaderT env # shouldReturnRight
        rc <- Repo.mockRepoContent mockRepo
        case A.head rc of
          Nothing -> fail "repo is empty"
          Just sub -> do
            sub.confirmed `shouldEqual` true
            unsubToken <- UC.sendNotificationMail sub [] "from" # flip runReaderT env # shouldReturnRight
            UC.unsubscribe unsubToken # flip runReaderT env # shouldReturnRight
            Repo.mockRepoContent mockRepo `shouldReturn` []
    describe "OfdbApi" do
      it "ofdbapi" do
        let api = OfdbApiRest { baseUrl: "https://api.ofdb.io/v0" }
        -- let query = defaultRcQuery { since = Just $ mkDate 2021 8 13 12 0, withRatings = Just true }
        let query = defaultRcQuery { withRatings = Just true }
        _entries <- getEntriesRecentlyChanged query api # shouldReturnRight
        -- liftEffect $ log $ show entries
        pure unit
      it "recentlyChanged" do
        let api = OfdbApiRest { baseUrl: "https://api.ofdb.io/v0" }
        let recentlyChangedRepo = SingletonFileRepo "data/trc.json"
        let rcFiles = RecentlyChangedFiles { recentlyChangedRepo }
        updateFeed api rcFiles # shouldNotThrow

