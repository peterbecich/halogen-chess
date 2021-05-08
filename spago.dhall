{ name = "halogen-chess"
, dependencies =
  [ "aff"
  , "affjax"
  , "argonaut-aeson-generic"
  , "argonaut-codecs"
  , "argonaut-core"
  , "console"
  , "css"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "foreign-generic"
  , "halogen"
  , "halogen-css"
  , "maybe"
  , "newtype"
  , "prelude"
  , "profunctor-lenses"
  , "psci-support"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs", "app/**/*.purs" ]
}
