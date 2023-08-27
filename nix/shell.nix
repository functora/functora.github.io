with (import ./project.nix); let
  unst = import ./nixpkgs-unstable.nix;
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
        unst.gleam
        unst.erlang
        unst.elixir
        unst.ollama
        unst.alejandra
        newpkgs.nodePackages.prettier
        newpkgs.haskellPackages.cabal-fmt
        newpkgs.haskellPackages.cabal2nix
        newpkgs.nix-prefetch-git
        #
        # NOTE : HLS takes too much disk space
        # and time to build, maybe does not worth it.
        #
        pkgs.haskell-language-server
      ]
      ++ (import ./tools.nix)
      ++ (import ./../pub/dazzle/nix/tools.nix)
      ++ (import ./../pub/functora-hakyll/nix/tools.nix)
      ++ (import ./../pub/bitfinex-client/nix/tools.nix)
      ++ (import ./../pub/functora/nix/tools.nix)
      ++ (pkgs.lib.optional
        (builtins.pathExists ./../prv/nix/tools.nix)
        (import ./../prv/nix/tools.nix));
  }
