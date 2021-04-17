{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "affjax"
  , "argonaut"
  , "console"
  , "effect"
  , "express"
  , "formatters"
  , "js-timers"
  , "node-fs"
  , "nodemailer"
  , "now"
  , "psci-support"
  , "spec"
  , "kvm-welcome-email-shared"
  ]
, packages = ../packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
