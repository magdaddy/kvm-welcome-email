module Test.Main where

import ThisPrelude

import Effect.Aff (launchAff_)
import Test.QCTest (qcSpec)
import Test.Spec (Spec)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)
import Test.Subscription (subscriptionSpec)
import Test.Turf (turfSpec)

main :: Effect Unit
main = launchAff_ do
  runSpec [consoleReporter] do
    allSpecs

allSpecs :: Spec Unit
allSpecs = do
  turfSpec
  subscriptionSpec
  qcSpec

