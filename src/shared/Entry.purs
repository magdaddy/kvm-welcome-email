module KvmMail.Shared.Entry where

import ThisPrelude

import Data.Array as A
import Data.DateTime.Instant (Instant, unInstant)
import Data.Formatter.Number as Format.Number
import Data.JSDate (JSDate, fromTime, getTime, toUTCString)
import Data.Maybe (fromMaybe)
import Data.Time.Duration (Seconds, convertDuration)
import Partial.Unsafe (unsafePartial)

type OldBEntry
  = { id :: String
    , title :: String
    , created :: Number
    , version :: Int
    , contact_name :: Maybe String
    , email :: Maybe String
    , country :: Maybe String
    , lat :: Number
    , lng :: Number
    }

type Entry
  = { id :: String
    , created :: JSDate
    , version :: Int
    , title :: String
    , description :: String
    , lat :: Number
    , lng :: Number
    , license :: String
    , street :: Maybe String
    , zip :: Maybe String
    , city :: Maybe String
    , country :: Maybe String
    , state :: Maybe String
    , contactName :: Maybe String
    , email :: Maybe String
    , telephone :: Maybe String
    , homepage :: Maybe String
    , openingHours :: Maybe String
    , foundedOn :: Maybe String
    , categories :: Array Category
    , tags :: Array String
    , ratings :: Array String
    , imageUrl :: Maybe String
    , imageLinkUrl :: Maybe String
    , custom :: Array CustomLink
    }

data Category
  = NonProfit
  | Commercial
  | Event

type CustomLink
  = { url :: String
    , title :: Maybe String
    , description :: Maybe String
    }

showEntry :: Entry -> String
showEntry e =
  "Entry " <> e.id
    <> "\ncreated: "
    <> toUTCString e.created


data EntryKey
  = Description
  | License
  | Street
  | Zip
  | City
  | Country
  | State
  | ContactName
  | Email
  | Telephone
  | Homepage
  | OpeningHours
  | FoundedOn
  | Categories
  | Tags
  | ImageUrl
  | ImageLinkUrl
  | Custom
  | CustomLinkTitle
  | CustomLinkDescription

formatInstantUnix :: Instant -> String
formatInstantUnix instant = Format.Number.format formatter (instant # toSeconds # unwrap)
  where
  formatter = unsafePartial case Format.Number.parseFormatString "0" of Right x -> x

toSeconds :: Instant -> Seconds
toSeconds i = i # unInstant # convertDuration



type BEntry
  = { id :: String
    , created :: Number
    , version :: Int
    , title :: String
    , description :: String
    , lat :: Number
    , lng :: Number
    , license :: String
    , street :: Maybe String
    , zip :: Maybe String
    , city :: Maybe String
    , country :: Maybe String
    , state :: Maybe String
    , contact_name :: Maybe String
    , email :: Maybe String
    , telephone :: Maybe String
    , homepage :: Maybe String
    , opening_hours :: Maybe String
    , founded_on :: Maybe String
    , categories :: Array String
    , tags :: Array String
    , ratings :: Array String
    , image_url :: Maybe String
    , image_link_url :: Maybe String
    , custom :: Maybe (Array { url :: String, title :: Maybe String, description :: Maybe String })
  }

fromBEntry :: BEntry -> Entry
fromBEntry
  { id
  , created
  , version
  , title
  , description
  , lat
  , lng
  , license
  , street
  , zip
  , city
  , country
  , state
  , contact_name
  , email
  , telephone
  , homepage
  , opening_hours
  , founded_on
  , categories
  , tags
  , ratings
  , image_url
  , image_link_url
  , custom
  } =
  { id
  , created: fromTime (created * 1000.0)
  , version
  , title
  , description
  , lat
  , lng
  , license
  , street
  , zip
  , city
  , country
  , state
  , contactName: contact_name
  , email
  , telephone
  , homepage
  , openingHours: opening_hours
  , foundedOn: founded_on
  , categories: fromBCategories categories
  , tags
  , ratings
  , imageUrl: image_url
  , imageLinkUrl: image_link_url
  , custom: fromMaybe [] custom
  }

toBEntry :: Entry -> BEntry
toBEntry
  { id
  , created
  , version
  , title
  , description
  , lat
  , lng
  , license
  , street
  , zip
  , city
  , country
  , state
  , contactName
  , email
  , telephone
  , homepage
  , openingHours
  , foundedOn
  , categories
  , tags
  , ratings
  , imageUrl
  , imageLinkUrl
  , custom
  } =
  { id
  , created: (getTime created) / 1000.0
  , version
  , title
  , description
  , lat
  , lng
  , license
  , street
  , zip
  , city
  , country
  , state
  , contact_name: contactName
  , email
  , telephone
  , homepage
  , opening_hours: openingHours
  , founded_on: foundedOn
  , categories: map toBCategory categories
  , tags
  , ratings
  , image_url: imageUrl
  , image_link_url: imageLinkUrl
  , custom: Just custom
  }

fromBCategory :: String -> Maybe Category
fromBCategory id = case id of
  "2cd00bebec0c48ba9db761da48678134" -> Just NonProfit
  "77b3c33a92554bcf8e8c2c86cedd6f6f" -> Just Commercial
  "c2dc278a2d6a4b9b8a50cb606fc017ed" -> Just Event
  _ -> Nothing

fromBCategories :: Array String -> Array Category
fromBCategories bcats = A.mapMaybe fromBCategory bcats

toBCategory :: Category -> String
toBCategory cat = case cat of
  NonProfit -> "2cd00bebec0c48ba9db761da48678134"
  Commercial -> "77b3c33a92554bcf8e8c2c86cedd6f6f"
  Event -> "c2dc278a2d6a4b9b8a50cb606fc017ed"

fromOldBEntry :: OldBEntry -> BEntry
fromOldBEntry
  { id
  , created
  , version
  , title
  , lat
  , lng
  , contact_name
  , email
  , country
  } =
  { id
  , created
  , version
  , title
  , description: ""
  , lat
  , lng
  , license: ""
  , street: Nothing
  , zip: Nothing
  , city: Nothing
  , country
  , state: Nothing
  , contact_name
  , email
  , telephone: Nothing
  , homepage: Nothing
  , opening_hours: Nothing
  , founded_on: Nothing
  , categories: []
  , tags: []
  , ratings: []
  , image_url: Nothing
  , image_link_url: Nothing
  , custom: Nothing
  }
