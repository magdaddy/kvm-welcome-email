module WelcomeEmail.App.StatusPage where

import Prelude

import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.RemoteData (RemoteData(..))
import WelcomeEmail.App.Caps (class ManageSettings, class SendTestMail)
import WelcomeEmail.App.Data (Action(..), Slots, State)
import WelcomeEmail.App.Util (cls)


render :: forall m.
  MonadAff m =>
  ManageSettings m =>
  SendTestMail m =>
  State -> H.ComponentHTML Action Slots m
render state =
  HH.div [ cls "settings container is-max-desktop" ]
    [ HH.div [ cls "box mt-5" ]
        [ case state.isRunning of
            NotAsked -> HH.text "Not asked"
            Loading -> HH.text "Loading..."
            Failure e -> HH.text $ show e
            Success isRunning -> do
              let bclass = if isRunning then "is-primary" else "is-primary"
              let statusText = if isRunning then "running" else "not running"
              let bgClass = if isRunning then "has-background-success-dark" else "has-background-danger-dark"
              HH.div [ cls "is-flex is-align-items-center" ]
                [ HH.span [ cls "mr-2", HP.style "inline-block;" ]
                    [ HH.text $ "Status: " <> statusText ]
                , HH.span [ cls $ bgClass <> " mr-2", HP.style "height: 2rem; width: 2rem; border-radius: 50%; display: inline-block;" ]
                    [  ]
                , HH.button
                    [ cls $ "button " <> bclass, HE.onClick \_ -> ToggleRunning ]
                    [ HH.text if isRunning then "Stop" else "Start" ]
                ]
        ]
    ]


