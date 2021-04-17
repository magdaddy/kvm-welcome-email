module WelcomeEmail.App.Main where

import Prelude

import App.Home as Home
import Effect (Effect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import WelcomeEmail.App.AppM (runAppM)
import WelcomeEmail.App.Env (Env)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody

  let
    defaultEntry =
      { id: "4c20979fe0754e74875afa4308d73ce7"
      , title: "Slowtec GmbH"
      , created: 1234.0
      , version: 33
      }

    environment :: Env
    environment = { defaultEntry }

    rootComponent = H.hoist (runAppM environment) Home.component

  runUI rootComponent { defaultEntry } body
