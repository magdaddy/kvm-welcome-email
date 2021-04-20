module WelcomeEmail.Shared.Boundary where

import Data.Maybe (Maybe(..))
import WelcomeEmail.Shared.Entry (Entry)

type Email =
  { subject :: String
  , body :: String
  }

type AuthConfig =
  { user :: String
  , pass :: String
  }

type NodeMailerConfig =
  { host :: String
  , port :: Int
  , secure :: Boolean
  , auth :: AuthConfig
  }

type Settings
  = { nodeMailer :: NodeMailerConfig
    , defaultEntry :: Entry
    , senderAddress :: String
    }

defaultSettings :: Settings
defaultSettings =
  { nodeMailer:
      { auth:
          { pass: ""
          , user: ""
          }
      , host: "in-v3.mailjet.com"
      , port: 587
      , secure: false
      }
  , defaultEntry:
      { id: "4c20979fe0754e74875afa4308d73ce7"
      , title: "Slowtec GmbH"
      , created: 1234.0
      , version: 33
      , contact_name: Just "Slowtec"
      , email: Just "post@slowtec.de"
      }
  , senderAddress: ""
  }

type TestMailPayload = { emailAddr :: String }

type TestMailResponse = { error :: Maybe String }
