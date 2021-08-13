module WelcomeEmail.Server.Subscription.Entities where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.JSDate (JSDate)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested (type (/\))
import WelcomeEmail.Server.Services.Ofdb (EntryChange)

type Id = String

type EmailAddr = String

type Tag = String

type LatLng = { lat :: Number, lng :: Number }

type BBox = LatLng /\ LatLng

data Frequency
  = Hour
  | Day
  | Week

derive instance Eq Frequency
derive instance Generic Frequency _
instance Show Frequency where show = genericShow

data ChangeType
  = NewEntries
  | AllEntries

derive instance Eq ChangeType
derive instance Generic ChangeType _
instance Show ChangeType where show = genericShow

type Subscription =
  { id :: Id
  , email :: EmailAddr
  , bbox :: BBox
  , tags :: Array Tag
  , frequency :: Frequency
  , changeType :: ChangeType
  , confirmed :: Boolean
  , secret :: String
  , lastSent :: JSDate
  }


type ConfirmationToken = String

type UnsubscribeToken = String

type SubscriptionAndDigest =
  { sub :: Subscription
  , digest :: Array EntryChange
  }
