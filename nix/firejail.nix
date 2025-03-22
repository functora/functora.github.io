let
  misc = import ./misc.nix;
  pkgs = import ./nixpkgs.nix;
in rec {
  mkFirejailSimple = pkg: {
    "${pkg}" = {
      executable = "${pkgs."${pkg}"}/bin/${pkg}";
      profile = "${pkgs.firejail}/etc/firejail/${pkg}.profile";
    };
  };
  mkFirejailCustom = {
    pkg,
    dir ? null,
    net ? false,
    grp ? null,
    cfg ? "",
    profile ? mkFirejailProfile {inherit pkg dir net grp cfg;},
    exe,
  }: {
    "${pkg}" = {
      executable = exe;
      inherit profile;
    };
  };
  mkFirejailOffline = {
    pkg,
    exe,
  }:
    mkFirejailCustom {
      inherit pkg exe;
      profile = pkgs.writeText "${pkg}.local" ''
        no3d
        nosound
        apparmor
        caps.drop all
        machine-id
        net none
        netfilter
        nodvd
        nogroups
        noinput
        nonewprivs
        noprinters
        noroot
        notv
        nou2f
        novideo
        shell none

        disable-mnt
        seccomp
        x11 none

        dbus-system none
        dbus-user none

        restrict-namespaces
      '';
    };
  mkFirejailProfile = {
    pkg,
    dir,
    net,
    grp,
    cfg,
  }:
    pkgs.writeText "${pkg}.local" (
      ''
        include default.profile

        include disable-X11.inc
        include disable-common.inc
        include disable-devel.inc
        include disable-exec.inc
        include disable-interpreters.inc
        include disable-proc.inc
        include disable-programs.inc
        include disable-shell.inc
        include disable-write-mnt.inc
        include disable-xdg.inc

        # no3d
        # nosound
        apparmor
        caps.drop all
        machine-id
        ${
          if net
          then ""
          else "net none"
        }
        netfilter
        nodvd
        nogroups
        noinput
        nonewprivs
        noprinters
        noroot
        notv
        nou2f
        novideo
        shell none

        disable-mnt
        private ${
          if dir == null
          then ""
          else ''''${HOME}/.firejail/${dir}''
        }
        private-bin none
        private-cache
        private-cwd
        private-dev
        ${
          if net
          then ""
          else "private-etc none"
        }
        private-lib none
        private-opt none
        private-srv none
        private-tmp
        seccomp
        ${
          if net
          then ""
          else "x11 none"
        }

        dbus-system none
        dbus-user none

        restrict-namespaces
      ''
      + cfg
    );
  mkFirejailWrapper = {
    pkg,
    dir ? null,
    net ? false,
    grp ? null,
    cfg ? "",
    desktop ? null,
    profile ? mkFirejailProfile {inherit pkg dir net grp cfg;},
    extraArgs ? [],
    exe,
  }:
    pkgs.runCommand "firejail-wrap"
    {
      preferLocalBuild = true;
      allowSubstitutes = false;
      meta.priority = -1; # take precedence over non-firejailed versions
    }
    (
      let
        firejailArgs = pkgs.lib.concatStringsSep " " (
          extraArgs
          ++ (
            if grp == null
            then []
            else ["--join-or-start=${grp}"]
          )
          ++ (
            pkgs.lib.optional (profile != null) "--profile=${toString profile}"
          )
        );
      in
        ''
          command_path="$out/bin/${pkg}"
          mkdir -p $out/bin
          mkdir -p $out/share/applications
          cat <<'_EOF' >"$command_path"
          #! ${pkgs.runtimeShell} -e
          exec /run/wrappers/bin/firejail ${firejailArgs} -- ${toString exe} "$@"
          _EOF
          chmod 0755 "$command_path"
        ''
        + pkgs.lib.optionalString (desktop != null) ''
          substitute ${desktop} $out/share/applications/$(basename ${desktop}) \
            --replace ${exe} "$command_path"
        ''
    );
  mkFirejailService = {
    pkg,
    dir ? null,
    net ? false,
    grp ? null,
    cfg ? "",
    desktop ? null,
    profile ? mkFirejailProfile {inherit pkg dir net grp cfg;},
    extraArgs ? [],
    srv ? pkg,
    exe,
  }: let
    drv = mkFirejailWrapper {
      inherit
        pkg
        exe
        dir
        net
        cfg
        desktop
        profile
        extraArgs
        ;
    };
  in
    misc.mkService {
      inherit srv;
      mkExe = _: "${drv}/bin/${pkg}";
    };
}
