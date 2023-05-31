with (import ./project.nix);
let unst = import ./nixpkgs-unstable.nix;
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
    unst.erlang
    unst.elixir
    #
    # NOTE : HLS takes too much disk space
    # and time to build, maybe does not worth it.
    #
    pkgs.haskell-language-server
  ] ++ (import ./tools.nix)
    ++ (import ./../pub/sombra/nix/tools.nix)
    ++ (import ./../pub/functora-hakyll/nix/tools.nix)
    ++ (import ./../pub/bitfinex-client/nix/tools.nix)
    ++ (pkgs.lib.optional
         (builtins.pathExists ./../prv/nix/tools.nix)
         (import ./../prv/nix/tools.nix));
}
