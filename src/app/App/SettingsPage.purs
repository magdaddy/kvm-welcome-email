module WelcomeEmail.App.SettingsPage where

import Prelude

import React.Basic.DOM as R
import React.Basic.Hooks (Component, component)
import WelcomeEmail.App.SettingsForm (mkSettingsForm)


mkSettingsPage :: Component { }
mkSettingsPage = do
  settingsForm <- mkSettingsForm
  component "SettingsPage" \_props -> React.do
    pure $
      R.div
        { className: "page settings container is-max-desktop"
        , children:
          [ R.h1
              { className: "has-text-right has-text-white mr-5"
              , children: [ R.text "Settings" ]
              }
          , settingsForm {}
          ]
        }


