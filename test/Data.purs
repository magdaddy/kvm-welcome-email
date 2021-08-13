module Test.Data where

import Data.Tuple.Nested ((/\))
import Test.Util (mkDate)
import WelcomeEmail.Server.Subscription.Entities (BBox, ChangeType(..), Frequency(..), Subscription)


bboxLubuMitteLinks = { lng: 9.1837, lat: 48.8891 } /\ { lng: 9.1941, lat: 48.9021 } :: BBox
bboxLubuMitteRechts = { lng: 9.1950, lat: 48.8891 } /\ { lng: 9.2118, lat: 48.9021 } :: BBox
bboxLubuWest = { lng: 9.1655, lat: 48.8891 } /\ { lng: 9.1835, lat: 48.9021 } :: BBox

rapunzelMitteLinks = { id: "idRapunzel", lat: 48.8987, lng: 9.1869 }
weltenkuecheWest = { id: "idWeltenkueche", lat: 48.8999, lng: 9.1738 }
urbanharbourWest = { id: "idUrbanharbour", lat: 48.8907, lng: 9.1732 }


subMitteLinks :: Subscription
subMitteLinks =
  { id: "idMitteLinks"
  , bbox: bboxLubuMitteLinks
  , email: "mitteLinks@lubu.com"
  , tags: []
  , frequency: Day
  , changeType: NewEntries
  , confirmed: true
  , secret: "secretMitteLinks"
  , lastSent: mkDate 2021 7 12 9 15
  }
subMitteRechts :: Subscription
subMitteRechts =
  { id: "idMitteRechts"
  , bbox: bboxLubuMitteRechts
  , email: "mitteRechts@lubu.com"
  , tags: []
  , frequency: Day
  , changeType: NewEntries
  , confirmed: true
  , secret: "secretMitteRechts"
  , lastSent: mkDate 2021 7 12 9 15
  }
subWest :: Subscription
subWest =
  { id: "idWest"
  , bbox: bboxLubuWest
  , email: "west@lubu.com"
  , tags: []
  , frequency: Day
  , changeType: NewEntries
  , confirmed: true
  , secret: "secretWest"
  , lastSent: mkDate 2021 7 12 9 15
  }
