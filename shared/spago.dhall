{ name = "my-project"
, dependencies =
  [ "console"
  , "effect"
  , "psci-support"
  , "formatters"
  , "argonaut"
  ]
, packages = ../packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
