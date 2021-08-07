module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec (Spec)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)
import Test.Subscription (subscriptionSpec)
import Test.Turf (turfSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  allSpecs

allSpecs :: Spec Unit
allSpecs = do
  turfSpec
  subscriptionSpec

