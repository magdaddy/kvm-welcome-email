module KvmMail.Server.Services.CrudRepo where

import ThisPrelude

import Data.Array as A
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Traversable (traverse)
import Effect.Aff (try)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile, writeTextFile)
import Simple.JSON (readJSON_)
import KvmMail.Shared.Boundary (class SerDe, deSer, ser)
import KvmMail.Shared.Util (writeJSONPretty)


data Error
  = OtherError String
  | NotFoundError
  | ImpossibleError String

derive instance Eq Error
derive instance Generic Error _
instance Show Error where show = genericShow


class Eq id <= HasId a id | a -> id where
  id :: a -> id

class CrudRepo repo item id | repo -> item, item -> id where
  read :: forall m. MonadAff m =>
    id -> repo -> ExceptT Error m item
  readAll :: forall m. MonadAff m =>
    repo -> ExceptT Error m (Array item)
  create :: forall m. MonadAff m =>
    item -> repo -> ExceptT Error m Unit
  update :: forall m. MonadAff m =>
    item -> repo -> ExceptT Error m Unit
  delete :: forall m. MonadAff m =>
    id -> repo -> ExceptT Error m Unit


newtype FileRepo = FileRepo String

loadItems :: forall m item bItem.
  MonadAff m => SerDe item bItem =>
  String -> ExceptT Error m (Array item)
loadItems fn = do
  res <- liftAff $ try $ readTextFile UTF8 fn
  let content = case res of
                  Left _ -> "[]"
                  Right c -> c
  bItems :: Array bItem <- except $ note (OtherError "Items could not be loaded: json error") $ readJSON_ content
  except $ note (OtherError "Items could not be loaded: from boundary error") $ traverse deSer bItems

saveItems :: forall m item bItem.
  MonadAff m => SerDe item bItem =>
  Array item -> String -> ExceptT Error m Unit
saveItems items fn = do
  liftAff $ writeTextFile UTF8 fn $ writeJSONPretty 2 $ map ser items
  pure unit

instance (HasId item id, SerDe item bItem) => CrudRepo (FileRepo) item id where
  read id' (FileRepo fn) = do
    items <- loadItems fn
    item <- except $ note NotFoundError $ A.find (\item -> id item == id') items
    pure item
  readAll (FileRepo fn) = do
    loadItems fn
  create item (FileRepo fn) = do
    items <- loadItems fn
    saveItems (A.snoc items item) fn
  update item (FileRepo fn) = do
    items <- loadItems fn
    idx <- except $ note NotFoundError $ A.findIndex (\item' -> id item' == id item) items
    newSubs <- except $ note (ImpossibleError "Update: Index out of bounds") $ A.updateAt idx item items
    saveItems newSubs fn
  delete id' (FileRepo fn) = do
    items :: Array item <- loadItems fn
    idx <- except $ note NotFoundError $ A.findIndex (\item -> id item == id') items
    newSubs <- except $ note (ImpossibleError "Delete: Index out of bounds") $ A.deleteAt idx items
    saveItems newSubs fn


newtype Mock item = Mock (Ref (Array item))

mkMock :: forall m item. MonadEffect m => Array item -> m (Mock item)
mkMock items = liftEffect $ Ref.new items >>= pure <<< Mock

mockRepoContent :: forall m item. MonadEffect m => Mock item -> m (Array item)
mockRepoContent (Mock mockRepoRef) = liftEffect $ Ref.read mockRepoRef

instance HasId item id => CrudRepo (Mock item) item id where
  read id' (Mock repoRef) = do
    items <- liftEffect $ Ref.read repoRef
    except $ note NotFoundError $ A.find (\item -> id item == id') items
  readAll (Mock repoRef) = do
    items <- liftEffect $ Ref.read repoRef
    pure items
  create item (Mock repoRef) = do
    liftEffect $ Ref.modify_ (flip A.snoc item) repoRef
    pure unit
  update item (Mock repoRef) = do
    items <- liftEffect $ Ref.read repoRef
    idx <- except $ note NotFoundError $ A.findIndex (\item' -> id item' == id item) items
    newSubs <- except $ note (ImpossibleError "Update: Index out of bounds") $ A.updateAt idx item items
    liftEffect $ Ref.write newSubs repoRef
  delete id' (Mock repoRef) = do
    items <- liftEffect $ Ref.read repoRef
    idx <- except $ note NotFoundError $ A.findIndex (\item -> id item == id') items
    newSubs <- except $ note (ImpossibleError "Delete: Index out of bounds") $ A.deleteAt idx items
    liftEffect $ Ref.write newSubs repoRef

