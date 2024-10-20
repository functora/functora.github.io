let
  repo = builtins.toString ./..;
in
  with (import ./project.nix); rec {
    pkgBin = pkg: "${pkgs.${pkg}}/bin/${pkg}";
    nix-bundle = pkgs.nix-bundle.overrideAttrs (attrs: {
      postInstall =
        attrs.postInstall
        + ''
          sed -i "s/g++/g++ -static-libstdc++/" $out/share/nix-bundle/nix-user-chroot/Makefile
        '';
    });
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
          }" --command="${pkgs.cabal-install}/bin/cabal new-repl ${
            if cmd == "lib"
            then pkg
            else "${pkg}-${cmd}"
          } --disable-optimization --repl-options=-fobject-code --repl-options=-fno-break-on-exception --repl-options=-fno-break-on-error --repl-options=-v1 --repl-options=-ferror-spans --repl-options=-j -fghcid")
        '';
      };
    mkService = srv: exe: {
      lib,
      pkgs,
      config,
      ...
    }:
      with lib; {
        imports = [
        ];

        options = {
          services."${srv}" = {
            enable = mkOption {
              default = false;
              type = types.bool;
            };
          };
        };

        config = mkIf config.services."${srv}".enable {
          systemd.user.services."${srv}" = {
            wants = ["network.target"];
            wantedBy = ["default.target"];
            script = "PATH=$PATH:${pkgs.busybox}/bin ${exe}";
            serviceConfig.Restart = "on-failure";
          };
        };
      };
  }
