module WelcomeEmail.App.SettingsPage where

import Prelude

import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Type.Proxy (Proxy(..))
import WelcomeEmail.App.Caps (class ManageSettings, class SendTestMail)
import WelcomeEmail.App.Data (Action, Slots, State)
import WelcomeEmail.App.SettingsForm as SettingsForm
import WelcomeEmail.App.Util (cls)


render :: forall m.
  MonadAff m =>
  ManageSettings m =>
  SendTestMail m =>
  State -> H.ComponentHTML Action Slots m
render state =
  HH.div [ cls "settings container is-max-desktop" ]
    [ HH.h1 [ cls "has-text-right has-text-white mr-5" ] [ HH.text "Settings" ]
    , HH.slot_ (Proxy :: _ "settingsForm") 0 SettingsForm.component unit
    ]


