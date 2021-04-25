{ name = "halogen-chess"
, dependencies =
  [ "argonaut-aeson-generic"
  , "argonaut-codecs"
  , "console"
  , "effect"
  , "foreign-generic"
  , "maybe"
  , "newtype"
  , "prelude"
  , "profunctor-lenses"
  , "psci-support"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
