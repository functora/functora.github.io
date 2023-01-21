with (import ./project.nix);
let nixpkgs = import ./nixpkgs.nix;
in
shellFor {
  exactDeps = false;
  withHoogle = false;
  tools = {
    hspec-discover = "latest";
  };
  buildInputs = [
    pkgs.cabal-install
    pkgs.ormolu
    pkgs.hpack
    pkgs.hlint
    pkgs.ghcid
    #
    # NOTE : HLS takes too much disk space
    # and time to build, does not worth it.
    #
    # pkgs.haskell-language-server
  ] ++ (import ./tools.nix)
    ++ (import ./../pub/functora-hakyll/nix/tools.nix)
    ++ (import ./../pub/bitfinex-client/nix/tools.nix);
}
