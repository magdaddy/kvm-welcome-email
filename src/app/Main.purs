module WelcomeEmail.App.Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Exception (throw)
import Web.DOM.Document (toParentNode)
import Web.DOM.ParentNode (QuerySelector(..), querySelector)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toDocument)
import Web.HTML.Window (document)
import WelcomeEmail.App.App (mkApp)
import React.Basic.DOM as R

-- main :: Effect Unit
-- main = HA.runHalogenAff do
--   body <- HA.awaitBody

--   let
--     environment :: Env
--     environment = {}

--     rootComponent = H.hoist (runAppM environment) Home.component

--   runUI rootComponent { defaultEntry: defaultSettings.defaultEntry } body

main :: Effect Unit
main = do
  document <- document =<< window
  root <- querySelector (QuerySelector "#root") $ toParentNode $ toDocument document
  case root of
    Nothing -> throw "Could not find #root element."
    Just r -> do
      app <- mkApp
      -- let chakraApp = chakraProvider_ [ app unit ]
      -- R.render chakraApp r
      R.render (app unit) r
