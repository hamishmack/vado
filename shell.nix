{ sourcesOverride ? {}, compiler-nix-name ? "ghc883" }:
(import ./. { inherit sourcesOverride compiler-nix-name; }).shellFor {
  tools = { cabal = "3.2.0.0"; };
}
