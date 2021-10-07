module KvmMail.App.Data where

import ThisPrelude

import Affjax as AX
import Data.Array as A
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.String as S
import Effect.Exception as Exn
import Foreign (MultipleErrors, renderForeignError)


data Page
  = StatusPage
  | TemplatePage
  | SettingsPage
  | LoginPage
  | RecentlyChangedPage

derive instance Eq Page
derive instance Generic Page _
instance Show Page where
  show = genericShow

data AppError
  = HttpError AX.Error
  | JsonError MultipleErrors
  | JsError Exn.Error
  | ServerError String
  | Unauthorized String
  | OtherError String

instance Show AppError where
  show = case _ of
    HttpError err -> "AffjaxError: " <> AX.printError err
    JsonError err -> "JsonError: " <> (S.joinWith "\n" <<< A.fromFoldable <<< (map renderForeignError)) err
    JsError err -> "JsError: " <> Exn.message err
    ServerError err -> "ServerError: " <>  err
    Unauthorized err -> "Unauthorized: " <>  err
    OtherError err -> "OtherError: " <> err

