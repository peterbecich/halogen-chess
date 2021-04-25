let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.1-20210419/packages.dhall sha256:d9a082ffb5c0fabf689574f0680e901ca6f924e01acdbece5eeedd951731375a

let additions =
      { argonaut-aeson-generic =
        { dependencies =
          [ "argonaut"
          , "argonaut-codecs"
          , "argonaut-generic"
          , "console"
          , "effect"
          , "foreign-object"
          , "psci-support"
          , "test-unit"
          ]
        , repo =
            "git://github.com/peterbecich/purescript-argonaut-aeson-generic.git"
        , version = "2c8c5ee2381ddb786af7fb79a73e3b83001d68e8"
        }
      , css =
        { dependencies =
          [ "colors"
          , "console"
          , "effect"
          , "exceptions"
          , "nonempty"
          , "profunctor"
          , "psci-support"
          , "strings"
          , "these"
          , "transformers"
          ]
        , repo = "git://github.com/purescript-contrib/purescript-css.git"
        , version = "5c1a44ee95c259352a2b4570b060de14130540bc"
        }
      , halogen-css =
        { dependencies = [ "css", "halogen" ]
        , repo =
            "git://github.com/purescript-halogen/purescript-halogen-css.git"
        , version = "1600df2902eccc3e3c73559a3446c99469885af6"
        }
      }

in  upstream ⫽ additions
