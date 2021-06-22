module WelcomeEmail.Server.Data where

import Prelude

import Affjax as AX
import Effect.Exception as Exn
import Foreign (MultipleErrors)


data AppError
  = HttpError AX.Error
  | JsonError MultipleErrors
  | JsError Exn.Error
  | NodeMailerError Exn.Error
  | OtherError String

instance showAppError :: Show AppError where
  show = case _ of
    HttpError err -> "AffjaxError: " <> AX.printError err
    JsonError err -> "JsonError: " <> show err
    JsError err -> "JsError: " <> Exn.message err
    NodeMailerError err -> "NodeMailerError: " <> Exn.message err
    OtherError err -> "OtherError: " <> err
