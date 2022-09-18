let
  sources = import ./nix/sources.nix {};
  haskellNix = import sources.haskellNix {};
  pkgs = import
    haskellNix.sources.nixpkgs-2205
    haskellNix.nixpkgsArgs;
in pkgs.haskell-nix.project {
  projectFileName = "cabal.project";
  src = pkgs.haskell-nix.haskellLib.cleanGit {
    name = "halogen-chess";
    src = ./.;
  };
  compiler-nix-name = "ghc924";
}
