let
  pkgs = import ./nixpkgs.nix;
  nixpak = import ./nixpak.nix;
  mkNixPak = nixpak.lib.nixpak {
    inherit (pkgs) lib;
    inherit pkgs;
  };
  sandbox = mkNixPak {
    config = {sloth, ...}: {
      # the application to isolate
      app.package = pkgs.hello;

      # path to the executable to be wrapped
      # this is usually autodetected but
      # can be set explicitly nonetheless
      app.binPath = "bin/hello";

      # enabled by default, flip to disable
      # and to remove dependency on xdg-dbus-proxy
      dbus.enable = true;

      # same usage as --see, --talk, --own
      dbus.policies = {
        "org.freedesktop.DBus" = "talk";
        "ca.desrt.dconf" = "talk";
      };

      # needs to be set for Flatpak emulation
      # defaults to com.nixpak.${name}
      # where ${name} is generated from the drv name like:
      # hello -> Hello
      # my-app -> MyApp
      flatpak.appId = "org.myself.HelloApp";

      bubblewrap = {
        # disable all network access
        network = false;

        # lists of paths to be mounted inside the sandbox
        # supports runtime resolution of environment variables
        # see "Sloth values" below

        # bind.rw = [
        #   (sloth.concat' sloth.homeDir "/Documents")
        #   (sloth.env "XDG_RUNTIME_DIR")
        #   # a nested list represents a src -> dest mapping
        #   # where src != dest
        #   [
        #     (sloth.concat' sloth.homeDir "/.local/state/nixpak/hello/config")
        #     (sloth.concat' sloth.homeDir "/.config")
        #   ]
        # ];

        bind.ro = [
          (sloth.concat' sloth.homeDir "/Downloads")
        ];

        bind.dev = [
          "/dev/dri"
        ];
      };
    };
  };
in
  sandbox.config.script
