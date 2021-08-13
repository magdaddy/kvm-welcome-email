module WelcomeEmail.Shared.Util where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception.Unsafe (unsafeThrow)
import Foreign (Foreign)
import Partial.Unsafe (unsafeCrashWith)
import Simple.JSON (class WriteForeign, write)


eTodo :: forall a m. MonadEffect m => m a
eTodo = unsafeCrashWith "todo"

unwrapOrThrow :: forall a b. Show a => Either a b -> Effect b
unwrapOrThrow = case _ of
  Left err -> unsafeThrow $ show err
  Right result -> pure result

foreign import stringifyPretty :: Int -> Foreign -> String

writeJSONPretty :: forall a. WriteForeign a => Int -> a -> String
writeJSONPretty indent obj = stringifyPretty indent $ write obj

genId :: forall m. MonadEffect m => m String
genId = liftEffect genIdImpl
foreign import genIdImpl :: Effect String

genId16 :: forall m. MonadEffect m => m String
genId16 = liftEffect genId16Impl
foreign import genId16Impl :: Effect String
