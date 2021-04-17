{ name = "halogen-project"
, dependencies =
  [ "affjax"
  , "argonaut"
  , "console"
  , "effect"
  , "formatters"
  , "halogen"
  , "halogen-formless"
  , "psci-support"
  , "remotedata"
  , "kvm-welcome-email-shared"
  ]
, packages = ../packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
