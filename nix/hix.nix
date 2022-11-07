{pkgs, ...}: {
  name = "halogen-chess";
  compiler-nix-name = "ghc924"; # Version of GHC to use

  crossPlatforms = p: pkgs.lib.optionals pkgs.stdenv.hostPlatform.isx86_64 ([
    # p.mingwW64
    # p.ghcjs
  ] ++ pkgs.lib.optionals pkgs.stdenv.hostPlatform.isLinux [
    p.musl64
  ]);

  # shell.tools.hlint = "latest";
  shell.tools =
    { cabal = "latest";
      hlint = "latest";
       # haskell-language-server = "latest";
    };
}
