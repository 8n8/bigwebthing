{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "arraybuffer"
  , "console"
  , "effect"
  , "foreign"
  , "foreign-object"
  , "math"
  , "psci-support"
  , "localforage"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
