{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "timeline"
, dependencies =
  [ "data-default"
  , "debug"
  , "indexed-array"
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
  , "zeta-mapping"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
