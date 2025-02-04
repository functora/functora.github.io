with (import ./project.nix {}); let
  misc = import ./misc.nix;
  newpkgs = import ./newpkgs.nix;
in
  shellFor {
    tools = {
      cabal-install = "latest";
      hspec-discover = "latest";
      haskell-language-server = "latest";
      implicit-hie = "latest";
      cabal-fmt = "latest";
      ghcid = "latest";
      hpack = "latest";
    };
    buildInputs =
      [
        pkgs.hlint
        pkgs.gleam
        pkgs.erlang
        pkgs.elixir
        newpkgs.litecli
        misc.nix-bundle
      ]
      ++ (import ./tools.nix)
      ++ (import ./../pub/dazzle/nix/tools.nix)
      ++ (import ./../pub/functora-hakyll/nix/tools.nix)
      ++ (import ./../pub/functora/nix/tools.nix)
      ++ (import ./../pub/bfx/nix/tools.nix)
      ++ (pkgs.lib.optional
        (builtins.pathExists ./../prv/nix/tools.nix)
        (import ./../prv/nix/tools.nix));
  }
