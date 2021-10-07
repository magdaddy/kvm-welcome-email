module KvmMail.Shared.JsonCodecs where

import ThisPrelude

import Data.Argonaut.Core (Json, jsonNull)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Compat (maybe)
import Data.Codec.Argonaut.Migration as CAM
import Data.Codec.Argonaut.Record as CAR
import KvmMail.Shared.Boundary (Email, NodeMailerConfig, Settings)
import KvmMail.Shared.Entry (BEntry, Entry, fromBEntry, toBEntry)


jsonDecode :: forall a. CA.JsonCodec a -> Json -> Either CA.JsonDecodeError a
jsonDecode = CA.decode
jsonEncode :: forall a. CA.JsonCodec a -> a -> Json
jsonEncode = CA.encode


bEntryCdc :: CA.JsonCodec BEntry
bEntryCdc =
  CAM.addDefaultField "zip" jsonNull CA.>~>
  CAM.addDefaultField "telephone" jsonNull CA.>~>
  CAM.addDefaultField "street" jsonNull CA.>~>
  CAM.addDefaultField "state" jsonNull CA.>~>
  CAM.addDefaultField "opening_hours" jsonNull CA.>~>
  CAM.addDefaultField "image_url" jsonNull CA.>~>
  CAM.addDefaultField "image_link_url" jsonNull CA.>~>
  CAM.addDefaultField "homepage" jsonNull CA.>~>
  CAM.addDefaultField "founded_on" jsonNull CA.>~>
  CAM.addDefaultField "email" jsonNull CA.>~>
  CAM.addDefaultField "contact_name" jsonNull CA.>~>
  CAM.addDefaultField "city" jsonNull CA.>~>
  CAM.addDefaultField "country" jsonNull CA.>~>
    CAR.object "BEntry"
      { id: CA.string
      , created: CA.number
      , version: CA.int
      , title: CA.string
      , description: CA.string
      , lat: CA.number
      , lng: CA.number
      , license: CA.string
      , street: maybe CA.string
      , zip: maybe CA.string
      , city: maybe CA.string
      , country: maybe CA.string
      , state: maybe CA.string
      , contact_name: maybe CA.string
      , email: maybe CA.string
      , telephone: maybe CA.string
      , homepage: maybe CA.string
      , opening_hours: maybe CA.string
      , founded_on: maybe CA.string
      , categories: CA.array CA.string
      , tags: CA.array CA.string
      , ratings: CA.array CA.string
      , image_url: maybe CA.string
      , image_link_url: maybe CA.string
      , custom: maybe $ CA.array $ CAR.object "Custom"
          { url: CA.string, title: maybe CA.string, description: maybe CA.string }
      }

entryCdc :: CA.JsonCodec Entry
entryCdc = CA.prismaticCodec "Entry" (Just <<< fromBEntry) toBEntry bEntryCdc

emailCdc :: CA.JsonCodec Email
emailCdc = CAR.object "Email"
  { subject: CA.string
  , body: CA.string
  }

nodeMailerConfigCdc :: CA.JsonCodec NodeMailerConfig
nodeMailerConfigCdc = CAR.object "NodeMailerConfig"
  { host: CA.string
  , port: CA.int
  , secure: CA.boolean
  , auth: CAR.object "AuthConfig" { user: CA.string, pass: CA.string }
  }

settingsCdc :: CA.JsonCodec Settings
settingsCdc = CAR.object "Settings"
  { nodeMailer: nodeMailerConfigCdc
  , defaultEntry: entryCdc
  , senderAddress: CA.string
  }
