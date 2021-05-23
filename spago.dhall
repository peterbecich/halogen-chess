{ name = "halogen-chess"
, dependencies =
  [ "aff"
  , "affjax"
  , "argonaut-aeson-generic"
  , "argonaut-codecs"
  , "argonaut-core"
  , "arrays"
  , "console"
  , "css"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "foreign-generic"
  , "halogen"
  , "halogen-css"
  , "halogen-store"
  , "maybe"
  , "newtype"
  , "prelude"
  , "profunctor-lenses"
  , "psci-support"
  , "routing"
  , "routing-duplex"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs", "app/**/*.purs" ]
}
