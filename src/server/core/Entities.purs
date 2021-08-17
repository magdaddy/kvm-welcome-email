module WelcomeEmail.Server.Core.Entities where

import Data.JSDate (JSDate)
import Data.Maybe (Maybe)

type Entry
  = { id :: String
    , created :: JSDate
    , version :: Int
    , title :: String
    , contactName :: Maybe String
    , email :: Maybe String
    , country :: Maybe String
    , lat :: Number
    , lng :: Number
    }

