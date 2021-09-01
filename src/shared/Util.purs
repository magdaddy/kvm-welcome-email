module WelcomeEmail.Shared.Util where

import ThisPrelude

import Control.Monad.Except (runExceptT)
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

repeatMA :: forall m. MonadAff m => Int -> m Unit -> m Unit
repeatMA i loop
  | i > 0 = loop *> repeatMA (i - 1) loop
  | otherwise = pure unit

logExceptConsole :: forall e m. MonadEffect m => Show e => ExceptT e m Unit -> m Unit
logExceptConsole ex = do
  res <- runExceptT ex
  case res of
    Left err -> log $ show err
    Right _ -> pure unit
