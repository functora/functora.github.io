{
  compiler-nix-name = "ghc982";
  crossPlatforms = p: [p.ghcjs];
  shell.tools.cabal = "latest";
  shell.buildInputs =
            (let pkgs = import ./nixpkgs.nix;
             in [pkgs.ghc pkgs.nodejs-slim] ++ (import ./tools.nix));
}
