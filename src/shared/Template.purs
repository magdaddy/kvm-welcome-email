module WelcomeEmail.Shared.Template where

import Prelude

import Data.Argonaut (class EncodeJson)
import Data.Array (foldl)
import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Newtype (class Newtype)
import Data.String (Pattern(..), Replacement(..), replaceAll)
import Simple.JSON (class ReadForeign, class WriteForeign)
import Type.Prelude (Proxy(..))
import WelcomeEmail.Shared.Boundary (Email)
import WelcomeEmail.Shared.Entry (Entry, formatCreatedEmail)

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
  link = "https://kartevonmorgen.org/#/?entry=" <> entry.id
  patts = [ replaceAll (Pattern "{{title}}") (Replacement entry.title)
          , replaceAll (Pattern "{{created}}") (Replacement $ formatCreatedEmail entry.created)
          , replaceAll (Pattern "{{link}}") (Replacement link)
          ]
  applyPatts = foldl (<<<) identity patts

