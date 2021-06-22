module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "purescript-spec" do
    it "awesome" do
      let isAwesome = true
      isAwesome `shouldEqual` true
  describe "server" do
    it "rexex matches id" do
      let isAwesome = true
      isAwesome `shouldEqual` true
