{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "timeline"
, dependencies =
  [ "argonaut"
  , "arraybuffer-class"
  , "data-default"
  , "debug"
  , "indexed-multiset"
  , "indexed-demiset"
  , "indexed-set"
  , "node-fs-aff"
  , "numbers"
  , "psci-support"
  , "quickcheck-utf8"
  , "spec"
  , "stringutils"
  , "uuid"
  , "web-html"
  , "zeta"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
