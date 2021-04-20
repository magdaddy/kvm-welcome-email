module WelcomeEmail.App.StatusPage where

import Prelude

import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Type.Proxy (Proxy(..))
import WelcomeEmail.App.Caps (class ManageSettings, class SendTestMail)
import WelcomeEmail.App.Data (Action, Slots, State)
import WelcomeEmail.App.Util (cls)


render :: forall m.
  MonadAff m =>
  ManageSettings m =>
  SendTestMail m =>
  State -> H.ComponentHTML Action Slots m
render state =
  HH.div [ cls "settings container is-max-desktop" ]
    [ HH.div [ cls "box mt-5" ]
        [ HH.text "lala"
        ]
    ]


