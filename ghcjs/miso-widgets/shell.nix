with (import ./default.nix); let
  repo = toString ./.;
  functora-pkgs = import "${functora}/nix/nixpkgs.nix";
  functora-tools = import "${functora}/nix/tools.nix";
  miso-widgets-ghcid = functora-pkgs.writeScriptBin "miso-widgets-ghcid" ''
    (cd ${builtins.toString ./.} && ${functora-pkgs.ghcid}/bin/ghcid --test=":main --fail-fast --color -f failed-examples" --command="${functora-pkgs.cabal-install}/bin/cabal new-repl miso-widgets-test --disable-optimization --repl-options=-fobject-code --repl-options=-fno-break-on-exception --repl-options=-fno-break-on-error --repl-options=-v1 --repl-options=-ferror-spans --repl-options=-j -fghcid")
  '';
in
  dev.env.overrideAttrs (prev: {
    buildInputs =
      [
        miso-widgets-ghcid
        functora-pkgs.cabal-install
        functora-pkgs.closurecompiler
        functora-pkgs.clean-css-cli
        functora-pkgs.html-minifier
        functora-pkgs.simple-http-server
        functora-pkgs.pkg-config
        functora-pkgs.libwebp
        functora-pkgs.nodejs
        functora-pkgs.terser
        functora-pkgs.secp256k1
      ]
      ++ prev.buildInputs
      ++ pkgs.lib.lists.take (pkgs.lib.lists.length functora-tools - 1)
      functora-tools;
  })
