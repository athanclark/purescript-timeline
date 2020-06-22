{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "timeline"
, dependencies =
  [ "data-default"
  , "debug"
  , "indexed-demiset"
  , "indexed-multiset"
  , "indexed-set"
  , "node-fs-aff"
  , "numbers"
  , "psci-support"
  , "quickcheck-utf8"
  , "spec"
  , "stringutils"
  , "timeline-time"
  , "uuid"
  , "web-html"
  , "zeta"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
