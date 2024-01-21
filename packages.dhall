let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.10-20231023/packages.dhall
        sha256:b9a482e743055ba8f2d65b08a88cd772b59c6e2084d0e5ad854025fa90417fd4

let additions =
      { argonaut-aeson-generic =
        { dependencies =
          [ "argonaut"
          , "argonaut-codecs"
          , "argonaut-generic"
          , "console"
          , "effect"
          , "foreign-object"
          , "test-unit"
          ]
        , repo = "https://github.com/coot/purescript-argonaut-aeson-generic.git"
        , version = "4cee717e3e0003b76e699550f5fc35976901078c"
        }
      , foreign-generic =
        { dependencies =
          [ "effect"
          , "foreign"
          , "foreign-object"
          , "ordered-collections"
          , "exceptions"
          , "record"
          , "identity"
          ]
        , repo = "https://github.com/jsparkes/purescript-foreign-generic.git"
        , version = "844f2ababa2c7a0482bf871e1e6bf970b7e51313"
        }
      }

in  upstream // additions
