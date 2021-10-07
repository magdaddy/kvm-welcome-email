module KvmMail.App.SettingsPage where

import ThisPrelude

import React.Basic.DOM as R
import React.Basic.Hooks (Component, component)
import KvmMail.App.Data (Page)
import KvmMail.App.SettingsForm (mkSettingsForm)


mkSettingsPage :: Component { setPage :: Page -> Effect Unit }
mkSettingsPage = do
  settingsForm <- mkSettingsForm
  component "SettingsPage" \props -> React.do
    pure $
      R.div
        { className: "page settings container is-max-desktop"
        , children:
          [ R.h1
              { className: "has-text-right has-text-white mr-5"
              , children: [ R.text "Settings" ]
              }
          , settingsForm { setPage: props.setPage }
          ]
        }


