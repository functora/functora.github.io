let
  repo = builtins.toString ./..;
in
  with (import ./project.nix); rec {
    pkgBin = pkg: "${pkgs.${pkg}}/bin/${pkg}";
    mkGhcid = pkg: cmd: opt:
      pkgs.writeShellApplication {
        name = "${pkg}-ghcid-${cmd}";
        text = ''
          (cd ${repo}/pub/${pkg} && ghcid --test=":main ${
            if opt == null
            then
              if cmd == "test"
              then "--fail-fast --color -f failed-examples"
              else ""
            else opt
          }" --command="${pkgs.cabal-install}/bin/cabal new-repl ${pkg}-${cmd} --disable-optimization --repl-options=-fobject-code --repl-options=-fno-break-on-exception --repl-options=-fno-break-on-error --repl-options=-v1 --repl-options=-ferror-spans --repl-options=-j -fghcid")
        '';
      };
  }
