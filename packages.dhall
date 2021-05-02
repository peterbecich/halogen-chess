let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.1-20210427/packages.dhall sha256:edbb8f70232fb83895c7ce02f5d2b29f6ee1722f1a70fc58d3bc0ab0de18afe4

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
        , version = "9ece80aab68d7776a1baa9dfab00ac68c4b72afa"
        }
      , halogen-css =
        { dependencies = [ "css", "halogen" ]
        , repo =
            "git://github.com/purescript-halogen/purescript-halogen-css.git"
        , version = "dae531c089f13aff27b3f6a2185d01f100478590"
        }
      }

in  upstream ⫽ additions
