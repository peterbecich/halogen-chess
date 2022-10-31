{
  # This is a template created by `hix init`
  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    purs-nix.url = "github:purs-nix/purs-nix/ps-0.15";
    ps-tools.follows = "purs-nix/ps-tools";
  };

  outputs = { self, nixpkgs, flake-utils, haskellNix, purs-nix, ps-tools }@inputs:
    let
      supportedSystems = [
        "x86_64-linux"
      ];
    in
      flake-utils.lib.eachSystem supportedSystems (system:
        let
          overlays = [ haskellNix.overlay
                       (final: prev: {
                         hixProject =
                           final.haskell-nix.hix.project {
                             projectFileName = "cabal.project";
                             src = ./.;
                             evalSystem = "x86_64-linux";
                           };
                       })
                     ];

          dockerImage = pkgs.dockerTools.buildImage {
            name = "peterbecich/halogen-chess";
            tag = "latest";
            created = "now";
            contents = [ flake.packages."halogen-chess:exe:halogen-chess" ];
            config = {
              Cmd = [ "halogen-chess" ];
              Env = [
                "CLIENT_DIR=/data"
              ];
            };
          };


          pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
          flake = pkgs.hixProject.flake {};


          ps-tools = inputs.ps-tools.legacyPackages.${system};
          purs-nix = inputs.purs-nix { inherit system; };

          argonaut-aeson-generic = {
            # purs-nix-info.name = "foo";
            purs-nix-info = {
              name = "foo";
              dependencies =
                with purs-nix.ps-pkgs;
                [ argonaut
                  argonaut-codecs
                  argonaut-generic
                  console
                  effect
                  foreign-object
                  test-unit
                ];
            };
            # purs-nix-info.dependencies =
            #   with purs-nix.ps-pkgs;
            #   [ argonaut
            #     argonaut-codecs
            #     argonaut-generic
            #     console
            #     effect
            #     foreign-object
            #     test-unit
            #   ];

            src.git = {
              repo = "https://github.com/bentongxyz/purescript-argonaut-aeson-generic.git";
              rev = "dcd925179b37a2ac749b7a1f1ae72bb69746d886";
            };
          };

          foreign-generic = {

            purs-nix-info = {
              name = "bar";
              dependencies =
                with purs-nix.ps-pkgs;
                [ effect
                  foreign
                  foreign-object
                  ordered-collections
                  exceptions
                  record
                  identity
                ];
            };
            # purs-nix-info.dependencies =
            #   with purs-nix.ps-pkgs;
            #   [ effect
            #     foreign
            #     foreign-object
            #     ordered-collections
            #     exceptions
            #     record
            #     identity
            #   ];

            src.git = {
              repo = "https://github.com/jsparkes/purescript-foreign-generic.git";
              rev = "844f2ababa2c7a0482bf871e1e6bf970b7e51313";
            };
          };

          ps =
            purs-nix.purs
              { dependencies =
                  with purs-nix.ps-pkgs;
                  [ aff
                    affjax
                    affjax-web
                    argonaut-aeson-generic
                    argonaut-codecs
                    argonaut-core
                    arrays
                    colors
                    console
                    css
                    effect
                    either
                    foldable-traversable
                    foreign
                    foreign-generic
                    halogen
                    halogen-css
                    halogen-store
                    maybe
                    newtype
                    prelude
                    profunctor-lenses
                    routing
                    routing-duplex
                    tuples
                    unordered-collections
                  ];

                srcs = [ ./app ./src ];
                # dir = ./.;
              };

          purescriptBundle = ps.modules.Main.bundle {};

        in flake // {

          packages.default = flake.packages."halogen-chess:exe:halogen-chess";

          packages.dockerImage = dockerImage;

          packages.purescriptBundle = purescriptBundle;
        });


          # devShells.default =
          #   pkgs.mkShell
          #     { packages =
          #         with pkgs;
          #         [ entr
          #           nodejs
          #           (ps.command {})
          #           ps-tools.for-0_15.purescript-language-server
          #           purs-nix.esbuild
          #           purs-nix.purescript
          #         ];

          #       shellHook =
          #         ''
          #         alias watch="find src | entr -s 'echo bundling; purs-nix bundle'"
          #         '';
          #     };


  # --- Flake Local Nix Configuration ----------------------------
  nixConfig = {
    # This sets the flake to use the IOG nix cache.
    # Nix should ask for permission before using it,
    # but remove it here if you do not want it to.
    extra-substituters = ["https://cache.iog.io"];
    extra-trusted-public-keys = ["hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="];
    allow-import-from-derivation = "true";
  };
}
