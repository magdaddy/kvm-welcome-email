module Test.Subscription where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Test.Spec (Spec, describe, describeOnly, it)
import Test.Spec.Assertions (shouldEqual, shouldReturn)
import WelcomeEmail.Server.Subscription.Api (SubscribePayload, subscribeFlow)
import WelcomeEmail.Server.Subscription.Entities (ChangeType(..), Frequency(..), Subscription)
import WelcomeEmail.Server.Subscription.Repo (create, get, makeMockRepo, mockRepoContent)
import WelcomeEmail.Server.Subscription.Usecases (subscribe)

exSubscription :: Subscription
exSubscription =
  { id: "abcd"
  , bbox: { lat: 44.5, lng: 2.2 } /\ { lat: 44.5, lng: 2.2 }
  , email: "la@lu.com"
  , tags: []
  , frequency: Day
  , changeType: NewEntries
  , confirmed: false
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
    it "create works" do
      mockRepo <- makeMockRepo []
      void $ create exSubscription mockRepo
      mockRepoContent mockRepo `shouldReturn` [exSubscription]
    it "get works" do
      mockRepo <- makeMockRepo [exSubscription]
      res <- get exSubscription.id mockRepo
      res `shouldEqual` Right exSubscription
    it "subscribe works" do
      mockRepo <- makeMockRepo []
      let s = exSubscription
      res <- subscribe s.email s.bbox s.tags s.frequency s.changeType mockRepo
      case res of
        Left _ -> liftEffect $ throw ""
        Right id -> mockRepoContent mockRepo `shouldReturn` [s { id = id }]
    it "subscribeFlow works" do
      mockRepo <- makeMockRepo []
      let s = exSubPayload
      res <- subscribeFlow s mockRepo
      case res of
        Left _ -> liftEffect $ throw ""
        Right _ -> pure unit
