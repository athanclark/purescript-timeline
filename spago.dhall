{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "timeline"
, dependencies =
  [ "argonaut"
  , "arraybuffer-class"
  , "console"
  , "debug"
  , "effect"
  , "indexed-multiset"
  , "numbers"
  , "prelude"
  , "psci-support"
  , "spec"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
