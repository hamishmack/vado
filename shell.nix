{ haskellCompiler ? "ghc883" }:
(import ./. { inherit haskellCompiler; }).shellFor {}

