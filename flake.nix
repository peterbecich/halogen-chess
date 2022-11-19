{
  # This is a template created by `hix init`
  inputs = {
    hackageNix = {
      url = "github:input-output-hk/hackage.nix";
      flake = false;
    };
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    haskellNix = {
      url = "github:input-output-hk/haskell.nix";
      inputs.hackage.follows = "hackageNix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    flake-utils.url = "github:numtide/flake-utils";
    purs-nix.url = "github:purs-nix/purs-nix/ps-0.15";
    ps-tools.follows = "purs-nix/ps-tools";
  };

  outputs = { self, nixpkgs, flake-utils, haskellNix, hackageNix, purs-nix, ps-tools }@inputs:
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
            contents =
              [ flake.packages."halogen-chess:exe:halogen-chess"
                staticFiles
                purescriptBundleDist
              ];
            config = {
              Cmd = [ "halogen-chess" ];
              Env = [
                "CLIENT_DIR=/app/static"
              ];
              ExposedPorts = {
                "8080/tcp" = {};
              };
            };
          };

          pkgs = import nixpkgs
            { inherit system overlays; inherit (haskellNix) config; };
          flake = pkgs.hixProject.flake {};

          ps-tools = inputs.ps-tools.legacyPackages.${system};
          purs-nix = inputs.purs-nix {
            inherit system;
          };

          argonaut-aeson-generic = purs-nix.build {
            name = "argonaut-aeson-generic";
            info =
              { # version = "1.0.0";
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

            src.git = {
              repo = "https://github.com/bentongxyz/purescript-argonaut-aeson-generic.git";
              rev = "dcd925179b37a2ac749b7a1f1ae72bb69746d886";
            };
          };

          foreign-generic = purs-nix.build {
            name = "foreign-generic";
            info =
              { # version = "0.15";
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

            src.git = {
              # allRefs = true;
              repo = "https://github.com/peterbecich/purescript-foreign-generic.git";
              rev = "844f2ababa2c7a0482bf871e1e6bf970b7e51313";
              sha256 = "1df3n2yq8gmndldl0i1b3xqal50q12za61vgafdd13h1zf9cp3j3";
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

                srcs = [ "app" "src" ];
                dir = ./.;
              };

          haskell-language-server =
            pkgs.buildPackages.haskell-nix.tool "ghc925" "haskell-language-server" {};
          haskell-language-server-wrapper =
            pkgs.writeShellScriptBin "haskell-language-server-wrapper"
              ''${haskell-language-server}/bin/haskell-language-server "$@"'';

          staticFiles = pkgs.stdenv.mkDerivation {
            name = "bundle-static-files";
            src = ./static;
            installPhase = ''
              mkdir -p $out/app/static/
              ls
              cp -r * $out/app/static/
            '';
          };

          purescriptBundle = ps.modules.Main.bundle
            { incremental = true;

            };

          purescriptBundleDist = pkgs.runCommand "purs-nix-make-bundle"
            { buildInputs =
                [
                  (ps.command
                    { srcs = [ ./app ./src ];
                      bundle =
                        { main = true;
                          module = "Main";
                          esbuild = { format = "iife"; };
                        };
                    }) ];
            }
            ''
            mkdir -p $out/app/static/
            purs-nix compile
            purs-nix bundle
            cp main.js $out/app/static/main.js
            '';

        in flake // {

          packages =
            { default = flake.packages."halogen-chess:exe:halogen-chess";

              dockerImage = dockerImage;

              purescriptBundle = purescriptBundle;

              purescriptBundleDist = purescriptBundleDist;
            };

          devShells.default = pkgs.hixProject.shellFor {
            tools =
              { cabal = "latest";
                hlint = "latest";
                haskell-language-server = "latest";
                ghcid = "latest";
              };

            buildInputs =
              with pkgs;
              [ entr
                nodejs
                (ps.command {})
                ps-tools.for-0_15.purescript-language-server
                purs-nix.esbuild
                purs-nix.purescript
                haskell-language-server-wrapper
              ];
          };
        });

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
