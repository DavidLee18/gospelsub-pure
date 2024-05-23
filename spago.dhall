{ name = "gospelsub-pure"
, dependencies =
  [ "aff"
  , "aff-promise"
  , "argonaut"
  , "arrays"
  , "console"
  , "control"
  , "effect"
  , "either"
  , "halogen"
  , "integers"
  , "maybe"
  , "prelude"
  , "profunctor-lenses"
  , "read"
  , "strings"
  , "tuples"
  , "typelevel-prelude"
  , "web-events"
  , "web-html"
  , "web-uievents"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
