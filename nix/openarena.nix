let
  pkgs = import ./nixpkgs.nix;
  rat = pkgs.fetchurl {
    url = "https://github.com/rdntcntrl/ratarena_release/releases/download/v0.18.2/z-ratmod-v0.18.2.pk3";
    hash = "sha256-CvO2696RgeM/ovI4OSIiGqtljk9YwGDgLHhkLbfB+oU=";
  };
  omega = pkgs.fetchurl {
    url = "https://github.com/Bishop-333/OmegA-mod/releases/download/v3.3.8/z_omega-v3.3.8.pk3";
    hash = "sha256-2/PLMtJy0RJ/TonKtJASgXammaC8j6kAlOUZgAsgdBI=";
  };
in
  import ./bubble.nix {
    inherit pkgs;
    name = "openarena";
    text = ''
      ${pkgs.openarena}/bin/openarena \
        +set fs_homepath ~/.openarena \
        +set fs_game omega "$@"
    '';
    mkOverlay = sloth: [
      {
        path = ["bubblewrap" "tmpfs"];
        update = _: ["/tmp"];
      }
      {
        path = ["bubblewrap" "bind" "rw"];
        update = prev:
          prev
          ++ [
            [
              (sloth.mkdir (sloth.concat' sloth.homeDir "/openarena/.openarena/rat"))
              (sloth.concat' sloth.homeDir "/.openarena/rat")
            ]
            [
              (sloth.mkdir (sloth.concat' sloth.homeDir "/openarena/.openarena/omega"))
              (sloth.concat' sloth.homeDir "/.openarena/omega")
            ]
          ];
      }
      {
        path = ["bubblewrap" "bind" "ro"];
        update = prev:
          prev
          ++ [
            [
              (toString ../cfg/q3.cfg)
              (
                sloth.concat'
                sloth.homeDir
                "/.openarena/rat/autoexec.cfg"
              )
            ]
            [
              (toString ../cfg/q3.cfg)
              (
                sloth.concat'
                sloth.homeDir
                "/.openarena/omega/autoexec.cfg"
              )
            ]
            [
              (toString rat)
              (
                sloth.concat'
                sloth.homeDir
                "/.openarena/rat/z-ratmod-v0.18.2.pk3"
              )
            ]
            [
              (toString omega)
              (
                sloth.concat'
                sloth.homeDir
                "/.openarena/omega/z_omega-v3.3.8.pk3"
              )
            ]
          ];
      }
    ];
  }
