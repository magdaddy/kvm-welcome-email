{ sources = [ "src/**/*.purs", "test/**/*.purs" ]
, name = "my-project"
, dependencies =
  [ "kvm-welcome-email-server"
  , "kvm-welcome-email-app"
  , "kvm-welcome-email-shared"
  ]
, packages = ./packages.dhall
}
