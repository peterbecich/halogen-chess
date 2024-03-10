{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/haskell-updates";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    check-flake.url = "github:srid/check-flake";
    flake-root.url = "github:srid/flake-root";
    purescript-overlay.url = "github:thomashoneyman/purescript-overlay";
    purescript-overlay.inputs.nixpkgs.follows = "nixpkgs";

    # flake-utils.url = "github:numtide/flake-utils";
    purs-nix.url = "github:purs-nix/purs-nix";
    ps-tools.follows = "purs-nix/ps-tools";

    purescript-bridge.url =
      "github:eskimor/purescript-bridge/793cd8206ae777dd8f77245c1c8dbeb597c7828f";
    purescript-bridge.flake = false;


    argonaut-aeson-generic-repo.url = "github:coot/purescript-argonaut-aeson-generic/4cee717e3e0003b76e699550f5fc35976901078c";
    argonaut-aeson-generic-repo.flake = false;

    foreign-generic-repo.url = "github:peterbecich/purescript-foreign-generic/844f2ababa2c7a0482bf871e1e6bf970b7e51313";
    foreign-generic-repo.flake = false;
  };

  outputs = inputs@{ self
                   , nixpkgs
                   , flake-parts
                   , haskell-flake
                   , check-flake
                   , flake-root
                   , purescript-overlay
                   , purs-nix
                   , ps-tools
                   , purescript-bridge
                   , argonaut-aeson-generic-repo
                   , foreign-generic-repo
                   }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [
        haskell-flake.flakeModule
        flake-root.flakeModule
        check-flake.flakeModule
      ];

      # ps =
      #   purs-nix.purs
      #     { dependencies =
      #         [ "console"
      #           "effect"
      #           "prelude"
      #         ];

      #       dir = ./.;

      #       # foreign.Main.node_modules =
      #       #   npmlock2nix.node_modules { src = ./.; } + /node_modules;
      #     };

      perSystem = { self', pkgs, system, config,... }:
        let
          purs-nix = inputs.purs-nix { inherit system; };
          ps-tools = inputs.ps-tools.legacyPackages.${system};

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
              repo = "https://github.com/coot/purescript-argonaut-aeson-generic.git";
              rev = "4cee717e3e0003b76e699550f5fc35976901078c";
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

          ps = purs-nix.purs {
            dependencies = with purs-nix.ps-pkgs; [
              aff
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

          staticFiles = pkgs.stdenv.mkDerivation {
            name = "bundle-static-files";
            src = ./static;
            installPhase = ''
              mkdir -p $out/app/static/
              ls
              cp -r * $out/app/static/
            '';
          };
        in {

          # https://flake.parts/overlays#consuming-an-overlay
          _module.args.pkgs = import inputs.nixpkgs {
            inherit system;
            overlays = [
              purescript-overlay.overlays.default
            ];
          };


          haskellProjects.default = {
            basePackages = pkgs.haskellPackages;
            settings = {
              # halogen-chess.cabalFlags.offline = true;
              # purescript-bridge.check = false;
              brick.jailbreak = true;
              hls-stan-plugin.check = false;
              # https://community.flake.parts/haskell-flake/dependency#nixpkgs
              # floskell = { super, ... }:
              #   { custom = _: super.floskell_0_11_0; };
            };
            packages = {
              chessIO.source = "0.6.1.1";
              # brick.source = "2.3.1";
              brick.source = "0.73";
              vty-unix.source = "0.2.0.0";
              vty.source = "5.39";
              purescript-bridge.source = inputs.purescript-bridge;
            };
            devShell = {
              enable = true;
              tools = haskellPackages: {
                inherit (haskellPackages) zlib stylish-haskell;
              };
              hlsCheck.enable = false;
            };

            # exclude devShell, fixes duplicate definition
            autoWire = [ "packages" "apps" "checks" ];
          };

          devShells.default = pkgs.mkShell {
            inputsFrom = [
              config.haskellProjects.default.outputs.devShell
            ];
            buildInputs = with pkgs; [
              purs
              spago
              purs-tidy-bin.purs-tidy-0_10_0
              purs-backend-es
              purescript-language-server
              purs-nix.esbuild
              purs-nix.purescript
              (ps.command {})
            ];
          };

          # single file
          # packages.foo = ps.bundle {
          #   # incremental = true;
          #   esbuild = {
          #     outfile = "/usr/bin/main.js";
          #     format = "iife";
          #   };
          #   main = true;
          # };

          # https://github.com/purs-nix/purs-nix/blob/master/docs/derivations.md#app
          packages.foo = ps.app {
            name = "main.js";
            esbuild = {
              format = "iife";
            };
          };

          packages.bar = pkgs.stdenv.mkDerivation rec {
            name = "javascript bundle";
            src = self'.packages.foo;
            dontUnpack = true;
            installPhase = ''
              mkdir -p $out/app/static
              cp $src/bin/main.js  $out/app/static/main.js
            '';
          };

          packages.dockerImage = pkgs.dockerTools.buildImage {
            name = "peterbecich/halogen-chess";
            tag = "latest";
            created = "now";
            copyToRoot = pkgs.buildEnv {
              name = "image-root";
              pathsToLink = [ "/bin" "/app/static" ];
              paths = [
                self'.packages.halogen-chess
                staticFiles
                self'.packages.bar
              ];
              # paths
            };
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

          # https://www.ertt.ca/nix/shell-scripts/
          # need to provide `sos` via nixpkgs
          # localDevelopment =
          #   pkgs.writeShellScriptBin "my-script" ''
          #     echo "start server"
          #     sos . -p ".*\.hs" -e ".*\#.*\.hs" -c "echo 'hello'"
          #   '';

          packages.default = self'.packages.halogen-chess;
        };
    };
}
