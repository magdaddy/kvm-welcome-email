module Test.Util where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Except (ExceptT, runExceptT)
import Data.Either (Either(..))
import Data.Int (toNumber)
import Data.JSDate (JSDate, jsdate)
import Effect.Exception (Error, error)
import Test.Spec.Assertions (fail, shouldSatisfy)


resultShouldSatisfy :: forall m t.
  MonadThrow Error m => Show t =>
  m t -> (t -> Boolean) -> m Unit
resultShouldSatisfy m r = m >>= (_ `shouldSatisfy` r)

shouldNotThrow :: forall m e t. MonadThrow Error m => Show e => ExceptT e m t -> m t
shouldNotThrow m = do
  res <- runExceptT m
  case res of
    Left err -> (throwError <<< error) $ show err
    Right r -> pure r

shouldThrow :: forall m e t. MonadThrow Error m => Show t => ExceptT e m t -> m Unit
shouldThrow m = do
  res <- runExceptT m
  case res of
    Left _ -> pure unit
    Right r -> fail $ "Did not throw but returned " <> show r

mkDate :: Int -> Int -> Int -> Int -> Int -> JSDate
mkDate year month day hour minute = jsdate
  { year: toNumber year
  , month: toNumber month
  , day: toNumber day
  , hour: toNumber hour
  , minute: toNumber minute
  , second: 0.0
  , millisecond: 0.0
  }
