module WelcomeEmail.Shared.Util where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Exception.Unsafe (unsafeThrow)
import Foreign (Foreign)
import Simple.JSON (class WriteForeign, write)


unwrapOrThrow :: forall a b. Show a => Either a b -> Effect b
unwrapOrThrow = case _ of
  Left err -> unsafeThrow $ show err
  Right result -> pure result

foreign import stringifyPretty :: Int -> Foreign -> String

writeJSONPretty :: forall a. WriteForeign a => Int -> a -> String
writeJSONPretty indent obj = stringifyPretty indent $ write obj
