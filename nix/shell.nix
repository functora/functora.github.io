with (import ./project.nix {}); let
  bak = import ./bak.nix;
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
        pkgs.libwebp
        newpkgs.litecli
        bak.bak-status
        bak.bak-commit
        bak.bak-encrypt
        bak.bak-decrypt
      ]
      ++ (import ./tools.nix)
      ++ (import ./../pub/functora-hakyll/nix/tools.nix)
      ++ (import ./../pub/functora/nix/tools.nix)
      ++ (import ./../pub/bfx/nix/tools.nix)
      ++ (pkgs.lib.optional
        (builtins.pathExists ./../prv/nix/tools.nix)
        (import ./../prv/nix/tools.nix));
    shellHook = ''
      mkdir -p ${toString ../.}/bak
      mkdir -p ${toString ../.}/out
    '';
  }
