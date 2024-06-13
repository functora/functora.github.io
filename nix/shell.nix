with (import ./project.nix); let
  misc = import ./misc.nix;
  newpkgs = import ./newpkgs.nix;
in
  shellFor {
    exactDeps = false;
    withHoogle = false;
    tools = {
      hspec-discover = "latest";
    };
    buildInputs =
      [
        pkgs.cabal-install
        pkgs.hpack
        pkgs.hlint
        pkgs.ghcid
        pkgs.gleam
        pkgs.erlang
        pkgs.elixir
        pkgs.ollama
        newpkgs.haskellPackages.cabal2nix
        newpkgs.nix-prefetch-git
        newpkgs.litecli
        newpkgs.sqlite
        newpkgs.sqlite-web
        misc.nix-bundle
        functora.components.exes.elm2miso
      ]
      ++ (import ./tools.nix)
      ++ (import ./../pub/dazzle/nix/tools.nix)
      ++ (import ./../pub/functora-hakyll/nix/tools.nix)
      ++ (import ./../pub/bitfinex-client/nix/tools.nix)
      ++ (import ./../pub/functora/nix/tools.nix)
      ++ (import ./../pub/bfx/nix/tools.nix)
      ++ (pkgs.lib.optional
        (builtins.pathExists ./../prv/nix/tools.nix)
        (import ./../prv/nix/tools.nix));
  }
