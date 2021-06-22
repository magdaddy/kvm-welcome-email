module WelcomeEmail.App.Data where

import Prelude

import Affjax as AX
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Effect.Exception as Exn
import Foreign (MultipleErrors)
import Halogen as H
import Network.RemoteData (RemoteData)
import WelcomeEmail.Shared.Boundary (Settings)
import WelcomeEmail.Shared.Entry (Entry)
import WelcomeEmail.Shared.Template (EmailTemplate)


type OpaqueSlot id = forall query. H.Slot query Void id

type Slots =
  ( rawHtml :: OpaqueSlot Int
  , settingsForm :: OpaqueSlot Int
  , loginForm :: OpaqueSlot Int
  , testMail :: OpaqueSlot Int
  )

data Page
  = Status
  | Template
  | Settings
  | Login

derive instance genericPage :: Generic Page _
instance showPage :: Show Page where
  show = genericShow

type State =
  { isRunning :: RemoteData AppError Boolean
  , defaultEntry :: Entry
  , settings :: RemoteData AppError Settings
  , page :: Page
  , templatePage :: TemplatePageState
  }

type TemplatePageState =
  { edit :: Boolean
  , template :: RemoteData AppError EmailTemplate
  , templateEdited :: EmailTemplate
  }

type Contact = { name :: String, text :: String }

data Action
  = Initialize
  | ShowPage Page

  | ToggleRunning
  | GetServerState

  | GetTemplate
  | EditTemplateClicked
  | SaveTemplateClicked
  | CancelEditTemplateClicked
  | TemplateSubjectEdited String
  | TemplateBodyEdited String

  | GetSettings


data AppError
  = HttpError AX.Error
  | JsonError MultipleErrors
  | JsError Exn.Error
  | OtherError String

instance showAppError :: Show AppError where
  show = case _ of
    HttpError err -> "AffjaxError: " <> AX.printError err
    JsonError err -> "JsonError: " <> show err
    JsError err -> "JsError: " <> Exn.message err
    OtherError err -> "OtherError: " <> err

