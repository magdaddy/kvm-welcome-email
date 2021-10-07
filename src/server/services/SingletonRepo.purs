module KvmMail.Server.Services.SingletonRepo where

import ThisPrelude

import Data.Argonaut (jsonParser, stringifyWithIndent)
import Data.Codec.Argonaut as CA
import Effect.Aff (try)
import Effect.Exception (message, throw)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile, writeTextFile)
import Simple.JSON (readJSON_)
import KvmMail.Server.Services.CrudRepo (Error(..))
import KvmMail.Shared.Boundary (class SerDe, deSer, ser)
import KvmMail.Shared.JsonCodecs (jsonDecode, jsonEncode)
import KvmMail.Shared.Util (throwLeft, writeJSONPretty)


class SingRepo repo content | repo -> content where
  load :: forall m. MonadAff m => repo -> m content
  save :: forall m. MonadAff m => content -> repo -> m Unit

newtype SingFileRepo a = SingFileRepo { filename :: String, codec :: CA.JsonCodec a }

instance SingRepo (SingFileRepo content) content where
  load rep = loadContentA rep
  save content rep = saveContentA content rep

loadContentA :: forall m a. MonadAff m => SingFileRepo a -> m a
loadContentA (SingFileRepo { filename, codec }) = do
  content <- liftAff $ readTextFile UTF8 filename
  json <- throwLeft $ jsonParser content
  throwLeft $ lmap CA.printJsonDecodeError $ jsonDecode codec json

saveContentA :: forall m a. MonadAff m => a -> SingFileRepo a -> m Unit
saveContentA item (SingFileRepo { filename, codec }) =
  liftAff $ writeTextFile UTF8 filename $ stringifyWithIndent 2 $ jsonEncode codec item

newtype SingMemRoRepo a = SingMemRoRepo a

instance SingRepo (SingMemRoRepo content) content where
  load (SingMemRoRepo content) = pure content
  save _ _ = liftEffect $ throw "SingMemRoRepo is readonly!"


class SingletonRepo repo content | repo -> content where
  loadA :: forall m. MonadAff m =>
    repo -> ExceptT Error m content
  saveA :: forall m. MonadAff m =>
    content -> repo -> ExceptT Error m Unit


newtype SingletonFileRepo = SingletonFileRepo String

instance SerDe content bContent => SingletonRepo SingletonFileRepo content where
  loadA (SingletonFileRepo fn) = loadContent fn
  saveA content (SingletonFileRepo fn) = saveContent content fn


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
