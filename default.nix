{ pkgs ? import nixpkgs haskellNixpkgsArgs
, nixpkgs ? haskellNix + "/nixpkgs"
, haskellNixpkgsArgs ? import haskellNix
, haskellNix ? builtins.fetchTarball {
    url = "https://github.com/input-output-hk/haskell.nix/archive/1917908ae9fd0ee6d5729301bd3283d7bf6c6383.tar.gz";
    sha256 = "10qssfww3zwrvqnh9jxpxqqm6fv5qz592ivpx725nk2xfwrskqfk";
  }
, haskellCompiler ? "ghc883"
, system ? null
}:
let
  project = pkgs.haskell-nix.cabalProject {
    src = pkgs.haskell-nix.haskellLib.cleanGit { src = ./.; };
    ghc = pkgs.haskell-nix.compiler.${haskellCompiler};
  };
in project // {
  shellFor = args: (project.shellFor args).overrideAttrs (_: {
    buildInputs = [ pkgs.haskell-nix.cabal-install ];
  });
}
