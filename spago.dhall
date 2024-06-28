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
  , "lists"
  , "maybe"
  , "prelude"
  , "profunctor-lenses"
  , "quickcheck"
  , "read"
  , "strings"
  , "transformers"
  , "tuples"
  , "typelevel-prelude"
  , "web-events"
  , "web-html"
  , "web-uievents"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
