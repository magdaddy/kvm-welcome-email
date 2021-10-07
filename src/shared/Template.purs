module KvmMail.Shared.Template where

import ThisPrelude

import Data.Argonaut (class EncodeJson)
import Data.Array as A
import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.String (Pattern(..), Replacement(..), replaceAll)
import MagLibs.DateFns (format)
import Simple.JSON (class ReadForeign, class WriteForeign)
import Type.Prelude (Proxy(..))
import KvmMail.Shared.Boundary (Email)
import KvmMail.Shared.Entry (Entry)

newtype EmailTemplate = EmailTemplate Email

derive instance newtypeEmailTemplate :: Newtype EmailTemplate _
derive newtype instance showEmailTemplate :: Show EmailTemplate
derive newtype instance readForeignEmailTemplate :: ReadForeign EmailTemplate
derive newtype instance writeForeignEmailTemplate :: WriteForeign EmailTemplate
derive newtype instance encodeJsonEmailTemplate :: EncodeJson EmailTemplate

_EmailTemplateSubject :: Lens' EmailTemplate String
_EmailTemplateSubject = _Newtype <<< prop (Proxy :: _ "subject")

_EmailTemplateBody :: Lens' EmailTemplate String
_EmailTemplateBody = _Newtype <<< prop (Proxy :: _ "body")

expand :: Entry -> EmailTemplate -> Email
expand entry (EmailTemplate t) = { subject: applyPatts t.subject, body: applyPatts t.body }
  where
  patts = [ replaceAll (Pattern "{{title}}") (Replacement entry.title)
          -- , replaceAll (Pattern "{{created}}") (Replacement $ formatCreatedEmail entry.created)
          , replaceAll (Pattern "{{created}}") (Replacement dtFormatted)
          , replaceAll (Pattern "{{link}}") (Replacement $ entryLink entry)
          ]
  applyPatts = A.foldl (<<<) identity patts
  dtFormatted = format "dd.MM." entry.created <> " um " <> format "HH:mm" entry.created

entryLink :: Entry -> String
entryLink entry = "https://kartevonmorgen.org/#/?entry=" <> entry.id

-- formatCreatedEmail :: Number -> String
-- formatCreatedEmail created = case instant $ convertDuration $ Seconds created of
--   Nothing -> "---"
--   Just inst -> formatDtEmail $ toDateTime inst

-- formatDtEmail :: DateTime -> String
-- formatDtEmail dt  = (formatDateTime "DD.MM." dt) <> " um " <> (formatDateTime "HH:mm" dt) <> " Uhr"

-- formatDateTime :: String -> DateTime -> String
-- formatDateTime formatStr dt = Format.DateTime.format formatter dt
--   where
--   formatter = unsafePartial case Format.DateTime.parseFormatString formatStr of Right x -> x
