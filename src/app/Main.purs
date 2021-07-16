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


main :: Effect Unit
main = do
  document <- document =<< window
  root <- querySelector (QuerySelector "#root") $ toParentNode $ toDocument document
  case root of
    Nothing -> throw "Could not find #root element."
    Just r -> do
      app <- mkApp
      R.render (app unit) r
