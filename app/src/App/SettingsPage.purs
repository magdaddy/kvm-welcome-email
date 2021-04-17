module WelcomeEmail.App.SettingsPage where

import Prelude

import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy(..))
import WelcomeEmail.App.Caps (class ManageSettings, class SendTestMail)
import WelcomeEmail.App.Data (Action(..), Slots, State)
import WelcomeEmail.App.SettingsForm as SettingsForm
import WelcomeEmail.App.TestMail as TestMail


render :: forall m.
  MonadAff m =>
  ManageSettings m =>
  SendTestMail m =>
  State -> H.ComponentHTML Action Slots m
render state =
  HH.div
    [ HP.classes [ HH.ClassName "settings" ] ]
    [ HH.h1_ [ HH.text "Settings" ]
    , HH.slot_ (Proxy :: _ "settingsForm") 0 SettingsForm.component unit
    , HH.button [ HE.onClick \_ -> SendTestMailClicked ] [ HH.text "Send test mail" ]
    , HH.slot_ (Proxy :: _ "testMail") 0 TestMail.component unit
    ]


