module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)
import WelcomeEmail.Server.Util (isInAt, isInCh, isInDach, isInDe)

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "turf DACH tests" do
    it "Saarbrücken is in Germany" do
      let p = { lat: 49.23265, lng: 6.99619 }
      isInDe p `shouldEqual` true
      isInAt p `shouldEqual` false
      isInCh p `shouldEqual` false
      isInDach p `shouldEqual` true
    it "Basel is in Switzerland" do
      let p = { lat: 47.55814, lng: 7.58769 }
      isInDe p `shouldEqual` false
      isInAt p `shouldEqual` false
      isInCh p `shouldEqual` true
      isInDach p `shouldEqual` true
    it "Strasbourg is not in DACH" do
      let p = { lat: 48.583611, lng: 7.748056 }
      isInDe p `shouldEqual` false
      isInAt p `shouldEqual` false
      isInCh p `shouldEqual` false
      isInDach p `shouldEqual` false
    it "Bregenz is in Austria" do
      let p = { lat: 47.505, lng: 9.749167 }
      isInDe p `shouldEqual` false
      isInAt p `shouldEqual` true
      isInCh p `shouldEqual` false
      isInDach p `shouldEqual` true
