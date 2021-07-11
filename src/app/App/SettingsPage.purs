module WelcomeEmail.App.SettingsPage where

import Prelude

import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import React.Basic.DOM as R
import React.Basic.Hooks (Component, component)
import Type.Proxy (Proxy(..))
import WelcomeEmail.App.Caps (class ManageSettings, class SendTestMail)
import WelcomeEmail.App.Data (Action, Slots, State)
import WelcomeEmail.App.SettingsForm as SettingsForm
import WelcomeEmail.App.Util (cls)


mkSettingsPage :: Component { }
mkSettingsPage = do
  component "SettingsPage" \_props -> React.do
    -- (isRunning :: RemoteData AppError Boolean) /\ setIsRunning <- useState' NotAsked
    -- useEffectOnce do
    --   launchAff_ do
    --     liftEffect $ setIsRunning Loading
    --     result <- Api.serverState
    --     liftEffect $ setIsRunning $ fromEither $ rmap _.isRunning result
    --   pure mempty
    pure $
      R.div
        { className: "page settings container is-max-desktop"
        , children:
          [ R.h1
              { className: "has-text-right has-text-white mr-5"
              , children: [ R.text "Settings" ]
              }
          , R.div {}
          ]
        }

render :: forall m.
  MonadAff m =>
  ManageSettings m =>
  SendTestMail m =>
  State -> H.ComponentHTML Action Slots m
render _state =
  HH.div [ cls "settings container is-max-desktop" ]
    [ HH.h1 [ cls "has-text-right has-text-white mr-5" ] [ HH.text "Settings" ]
    , HH.slot_ (Proxy :: _ "settingsForm") 0 SettingsForm.component unit
    ]


