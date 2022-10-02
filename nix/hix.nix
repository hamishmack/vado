{pkgs, ...}: {
  name = "vado";
  compiler-nix-name = "ghc924";
  shell.tools.cabal = "latest";
}
