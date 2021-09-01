module WelcomeEmail.Server.Services.SingletonRepo where

import ThisPrelude

import Data.Either (note)
import Effect.Aff (try)
import Effect.Exception (message)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile, writeTextFile)
import Simple.JSON (readJSON_)
import WelcomeEmail.Server.Services.CrudRepo (Error(..))
import WelcomeEmail.Shared.Boundary (class SerDe, deSer, ser)
import WelcomeEmail.Shared.Util (writeJSONPretty)


class SingletonRepo repo content | repo -> content where
  load :: forall m. MonadAff m =>
    repo -> ExceptT Error m content
  save :: forall m. MonadAff m =>
    content -> repo -> ExceptT Error m Unit


newtype SingletonFileRepo = SingletonFileRepo String

instance SerDe content bContent => SingletonRepo SingletonFileRepo content where
  load (SingletonFileRepo fn) = loadContent fn
  save content (SingletonFileRepo fn) = saveContent content fn


loadContent :: forall m item bItem.
  MonadAff m => SerDe item bItem =>
  String -> ExceptT Error m item
loadContent fn = do
  content <- (liftAff $ try $ readTextFile UTF8 fn) >>= (withExceptT (\e -> OtherError $ "IO error: " <> message e) <<< except)
  bItems :: bItem <- except $ note (OtherError "Items could not be loaded: json error") $ readJSON_ content
  except $ note (OtherError "Items could not be loaded: from boundary error") $ deSer bItems

saveContent :: forall m item bItem.
  MonadAff m => SerDe item bItem =>
  item -> String -> ExceptT Error m Unit
saveContent item fn = do
  result <- liftAff $ try $ writeTextFile UTF8 fn $ writeJSONPretty 2 $ ser item
  withExceptT (\e -> OtherError $ "IO error: " <> message e) $ except result
