module KvmMail.Server.Data where

import ThisPrelude

import Affjax as AX
import Data.Array as A
import Data.String as S
import Effect.Exception as Exn
import Foreign (MultipleErrors, renderForeignError)


data AppError
  = HttpError AX.Error
  | JsonError MultipleErrors
  | JsError Exn.Error
  | NodeMailerError Exn.Error
  | Unauthorized String
  | InvalidInput String
  | OtherError String

instance showAppError :: Show AppError where
  show = case _ of
    HttpError err -> "AffjaxError: " <> AX.printError err
    JsonError err -> "JsonError: " <> (S.joinWith "\n" <<< A.fromFoldable <<< (map renderForeignError)) err
    JsError err -> "JsError: " <> Exn.message err
    NodeMailerError err -> "NodeMailerError: " <> Exn.message err
    Unauthorized err -> "Unauthorized: " <> err
    InvalidInput err -> "Invalid input: " <> err
    OtherError err -> "OtherError: " <> err


newtype AffjaxError = AffjaxError AX.Error

instance Show AffjaxError where
  show (AffjaxError err) = AX.printError err
