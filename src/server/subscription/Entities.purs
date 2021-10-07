module KvmMail.Server.Subscription.Entities where

import ThisPrelude

import Data.Generic.Rep (class Generic)
import Data.JSDate (JSDate, toUTCString)
import Data.Show.Generic (genericShow)
import KvmMail.Shared.Boundary (EntryChange)

type Id = String

type EmailAddr = String

type Tag = String

type LatLng = { lat :: Number, lng :: Number }

newtype BBox = BBoxInternal { minlat :: Number, minlng :: Number, maxlat :: Number, maxlng :: Number }
derive instance Eq BBox
derive newtype instance Show BBox

mkBBox :: LatLng -> LatLng -> BBox
mkBBox p1 p2 = BBoxInternal
  { minlat: min p1.lat p2.lat
  , minlng: min p1.lng p2.lng
  , maxlat: max p1.lat p2.lat
  , maxlng: max p1.lng p2.lng
  }

minlat :: BBox -> Number
minlat (BBoxInternal bb) = bb.minlat

maxlat :: BBox -> Number
maxlat (BBoxInternal bb) = bb.maxlat

minlng :: BBox -> Number
minlng (BBoxInternal bb) = bb.minlng

maxlng :: BBox -> Number
maxlng (BBoxInternal bb) = bb.maxlng

northEast :: BBox -> LatLng
northEast (BBoxInternal bb) = { lat: bb.maxlat, lng: bb.maxlng }

southWest :: BBox -> LatLng
southWest (BBoxInternal bb) = { lat: bb.minlat, lng: bb.minlng }

intersects :: BBox -> BBox -> Boolean
intersects bb1 bb2 = intlat && intlng
  where
  intlat = (minlat bb1 <= minlat bb2 && minlat bb2 <= maxlat bb1)
        || (minlat bb1 <= maxlat bb2 && maxlat bb2 <= maxlat bb1)
  intlng = (minlng bb1 <= minlng bb2 && minlng bb2 <= maxlng bb1)
        || (minlng bb1 <= maxlng bb2 && maxlng bb2 <= maxlng bb1)


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
  , title :: String
  , email :: EmailAddr
  , lang :: Lang
  , bbox :: BBox
  , tags :: Array Tag
  , frequency :: Frequency
  , changeType :: ChangeType
  , confirmed :: Boolean
  , secret :: String
  , lastSent :: JSDate
  , created :: JSDate
  }

showSubscription :: Subscription -> String
showSubscription s = "Subscription " <> s.id
  <> "\nlastSent: " <> toUTCString s.lastSent
  <> "\nbbox: " <> show s.bbox

data Lang
  = EN
  | DE

derive instance Eq Lang
derive instance Generic Lang _
instance Show Lang where show = genericShow

type ConfirmationToken = String

type UnsubscribeToken = String

type SubscriptionAndDigest =
  { sub :: Subscription
  , digest :: Array EntryChange
  }
