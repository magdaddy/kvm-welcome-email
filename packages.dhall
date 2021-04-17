{-
Welcome to your new Dhall package-set!

Below are instructions for how to edit this file for most use
cases, so that you don't need to know Dhall to use it.

## Warning: Don't Move This Top-Level Comment!

Due to how `dhall format` currently works, this comment's
instructions cannot appear near corresponding sections below
because `dhall format` will delete the comment. However,
it will not delete a top-level comment like this one.

## Use Cases

Most will want to do one or both of these options:
1. Override/Patch a package's dependency
2. Add a package not already in the default package set

This file will continue to work whether you use one or both options.
Instructions for each option are explained below.

### Overriding/Patching a package

Purpose:
- Change a package's dependency to a newer/older release than the
    default package set's release
- Use your own modified version of some dependency that may
    include new API, changed API, removed API by
    using your custom git repo of the library rather than
    the package set's repo

Syntax:
where `entityName` is one of the following:
- dependencies
- repo
- version
-------------------------------
let upstream = --
in  upstream
  with packageName.entityName = "new value"
-------------------------------

Example:
-------------------------------
let upstream = --
in  upstream
  with halogen.version = "master"
  with halogen.repo = "https://example.com/path/to/git/repo.git"

  with halogen-vdom.version = "v4.0.0"
-------------------------------

### Additions

Purpose:
- Add packages that aren't already included in the default package set

Syntax:
where `<version>` is:
- a tag (i.e. "v4.0.0")
- a branch (i.e. "master")
- commit hash (i.e. "701f3e44aafb1a6459281714858fadf2c4c2a977")
-------------------------------
let upstream = --
in  upstream
  with new-package-name =
    { dependencies =
       [ "dependency1"
       , "dependency2"
       ]
    , repo =
       "https://example.com/path/to/git/repo.git"
    , version =
        "<version>"
    }
-------------------------------

Example:
-------------------------------
let upstream = --
in  upstream
  with benchotron =
      { dependencies =
          [ "arrays"
          , "exists"
          , "profunctor"
          , "strings"
          , "quickcheck"
          , "lcg"
          , "transformers"
          , "foldable-traversable"
          , "exceptions"
          , "node-fs"
          , "node-buffer"
          , "node-readline"
          , "datetime"
          , "now"
          ]
      , repo =
          "https://github.com/hdgarrood/purescript-benchotron.git"
      , version =
          "v7.0.0"
      }
-------------------------------
-}
let upstream =
    --   https://github.com/purescript/package-sets/releases/download/psc-0.14.0-20210308/packages.dhall sha256:5a86da7913f6c84adc2efacfad49ca135af8f62235e7270d9b952a8dda3c4b47
      -- https://github.com/purescript/package-sets/releases/download/psc-0.14.0-20210402/packages.dhall sha256:0cfaa5de499bd629f5263daff3261144d9d348d38a451b7938a6f52054c3a086
      https://github.com/purescript/package-sets/releases/download/psc-0.14.0-20210409/packages.dhall sha256:e81c2f2ce790c0e0d79869d22f7a37d16caeb5bd81cfda71d46c58f6199fd33f

in  upstream
  with express = ../../purs-fork/purescript-express/spago.dhall as Location
  with nodemailer = ../../purs-fork/purescript-nodemailer/spago.dhall as Location
  with kvm-welcome-email-server = ./server/spago.dhall as Location
  with kvm-welcome-email-app = ./app/spago.dhall as Location
  with kvm-welcome-email-shared = ./shared/spago.dhall as Location
  with convertable-options =
    { dependencies =
        [ "console", "effect", "maybe", "record" ]
    , repo =
        "https://github.com/natefaubion/purescript-convertable-options.git"
    , version =
        "v1.0.0"
    }
