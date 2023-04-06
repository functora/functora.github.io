with (import ./project.nix);
let unst = import ./nixpkgs-unstable.nix;
    obelisk = import (fetchTarball {
      url = "https://github.com/obsidiansystems/obelisk/archive/41f97410cfa2e22a4ed9e9344abcd58bbe0f3287.tar.gz";
      sha256 = "04bpzji7y3nz573ib3g6icb56s5zbj4zxpakhqaql33v2v77hi9g";
    }){};
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
    unst.gleam
    obelisk.command
    #
    # NOTE : HLS takes too much disk space
    # and time to build, maybe does not worth it.
    #
    pkgs.haskell-language-server
  ] ++ (import ./tools.nix)
    ++ (import ./../pub/functora-hakyll/nix/tools.nix)
    ++ (import ./../pub/bitfinex-client/nix/tools.nix)
    ++ (pkgs.lib.optional
         (builtins.pathExists ./../prv/nix/tools.nix)
         (import ./../prv/nix/tools.nix));
}
