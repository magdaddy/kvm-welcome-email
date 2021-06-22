module WelcomeEmail.App.LoginPage where

import Prelude

import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Type.Proxy (Proxy(..))
import WelcomeEmail.App.Data (Action, Slots, State)
import WelcomeEmail.App.Util (cls)
import WelcomeEmail.App.LoginForm as LoginForm


render :: forall m.
  MonadAff m =>
  State -> H.ComponentHTML Action Slots m
render state =
  HH.div [ cls "login container is-max-desktop" ]
    [ HH.div [ cls "box mt-5" ]
        [ HH.text "Login"
        , HH.slot_ (Proxy :: _ "loginForm") 0 LoginForm.component unit
        ]
    ]


