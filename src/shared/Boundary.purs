module KvmMail.Shared.Boundary where

import ThisPrelude

import Data.JSDate (JSDate, fromTime, getTime)
import Math as Math
import Simple.JSON (class ReadForeign, class WriteForeign)
import Test.Util (mkDate)
import KvmMail.Shared.Entry (BEntry, Entry, OldBEntry, fromBEntry, fromOldBEntry, toBEntry)

type Email =
  { subject :: String
  , body :: String
  }

type AuthConfig =
  { user :: String
  , pass :: String
  }

type NodeMailerConfig =
  { host :: String
  , port :: Int
  , secure :: Boolean
  , auth :: AuthConfig
  }

type Settings
  = { nodeMailer :: NodeMailerConfig
    , defaultEntry :: Entry
    , senderAddress :: String
    }

defaultSettings :: Settings
defaultSettings =
  { nodeMailer:
      { auth:
          { pass: ""
          , user: ""
          }
      , host: "in-v3.mailjet.com"
      , port: 587
      , secure: false
      }
  , defaultEntry:
      { id: "4c20979fe0754e74875afa4308d73ce7"
      , title: "Slowtec GmbH"
      , description: ""
      , created: mkDate 2017 4 3 12 0
      , version: 33
      , license: ""
      , contactName: Just "Slowtec"
      , email: Just "post@slowtec.de"
      , lat: 37.2
      , lng: 120.7
      , country: Nothing
      , street: Nothing
      , zip: Nothing
      , city: Nothing
      , state: Nothing
      , telephone: Nothing
      , homepage: Nothing
      , openingHours: Nothing
      , foundedOn: Nothing
      , categories: []
      , tags: []
      , ratings: []
      , imageUrl: Nothing
      , imageLinkUrl: Nothing
      , custom: []
      }
  , senderAddress: ""
  }

type BSettings
  = { nodeMailer :: NodeMailerConfig
    , defaultEntry :: BEntry
    , senderAddress :: String
    }
type OldBSettings
  = { nodeMailer :: NodeMailerConfig
    , defaultEntry :: OldBEntry
    , senderAddress :: String
    }

fromBSettings :: BSettings -> Settings
fromBSettings { nodeMailer, defaultEntry, senderAddress } = { nodeMailer, defaultEntry: fromBEntry defaultEntry, senderAddress }

fromOldBSettings :: OldBSettings -> Settings
fromOldBSettings { nodeMailer, defaultEntry, senderAddress } = { nodeMailer, defaultEntry: (fromBEntry <<< fromOldBEntry) defaultEntry, senderAddress }

toBSettings :: Settings -> BSettings
toBSettings { nodeMailer, defaultEntry, senderAddress } = { nodeMailer, defaultEntry: toBEntry defaultEntry, senderAddress }

type TestMailPayload = { emailAddr :: String }

type TestMailResponse = { error :: Maybe String }

type LoginData = { username :: String , password :: String }

type ServerErrorResponse = { error :: Maybe String }

type ErrorResponse = { error :: String }

type TokenResponse = { token :: String }

type LastLogEntry = { timestamp :: JSDate, type :: LastLogType }

data LastLogType
  = Error String
  | Warn String
  | EmailSent Entry

type LogLine =
  { timestamp :: String
  , level :: String
  , message :: String
  , wasSent :: Maybe Boolean
  , entry :: Maybe BEntry
  }
type OldLogLine =
  { timestamp :: String
  , level :: String
  , message :: String
  , wasSent :: Maybe Boolean
  , entry :: Maybe OldBEntry
  }

fromOldLogLine :: OldLogLine -> LogLine
fromOldLogLine ll = ll { entry = map fromOldBEntry ll.entry }

class (ReadForeign bItem, WriteForeign bItem) <= SerDe item bItem | item -> bItem where
  ser :: item -> bItem
  deSer :: bItem -> Maybe item

type EntryChange =
  { changed :: JSDate
  , entry :: Entry
  }

-- showEntryChange :: EntryChange -> String
-- showEntryChange ec = "changed: " <> toUTCString ec.changed
--   <> "\n" <> showEntry ec.entry


instance SerDe EntryChangeA (Array BEntryChange) where
  ser (EntryChangeA entries) =
    (\ec -> { changed: Math.round ((getTime ec.changed) / 1000.0) , entry: toBEntry ec.entry }) <$> entries
  deSer bEntries = Just $ wrap $
    (\bec -> { changed: fromTime (bec.changed * 1000.0), entry: fromBEntry bec.entry }) <$> bEntries


newtype EntryChangeA = EntryChangeA (Array EntryChange)
derive instance Newtype EntryChangeA _

type BEntryChange =
  { changed :: Number
  , entry :: BEntry
  }

