module WelcomeEmail.App.Env where

import Prelude

type Env =
  { defaultEntry :: DefaultEntry
  }

type DefaultEntry =
  { id :: String
  , title :: String
  , created :: Number
  , version :: Int
  }
