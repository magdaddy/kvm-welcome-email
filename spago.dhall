{ name = "kvm-welcome-email"
, dependencies =
  [ "debug"
  , "affjax"
  , "argonaut"
  , "console"
  , "effect"
  , "express"
  , "formatters"
  , "halogen"
  , "halogen-formless"
  , "js-timers"
  , "node-fs"
  , "nodemailer"
  , "node-process"
  , "now"
  , "psci-support"
  , "remotedata"
  , "spec"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
