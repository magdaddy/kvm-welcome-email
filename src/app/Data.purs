module WelcomeEmail.App.Data where

import Prelude

import Affjax as AX
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Effect.Exception as Exn
import Foreign (MultipleErrors)


data Page
  = StatusPage
  | TemplatePage
  | SettingsPage
  | LoginPage

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
    JsonError err -> "JsonError: " <> show err
    JsError err -> "JsError: " <> Exn.message err
    ServerError err -> "ServerError: " <>  err
    Unauthorized err -> "Unauthorized: " <>  err
    OtherError err -> "OtherError: " <> err

