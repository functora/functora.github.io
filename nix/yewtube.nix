let
  pkgs = import ./nixpkgs.nix;
in
  with pkgs.python3Packages; let
    pyreadline = buildPythonPackage rec {
      pname = "pyreadline";
      version = "2.1";
      doCheck = false;
      propagatedBuildInputs = [];
      src = fetchPypi {
        inherit pname version;
        extension = "zip";
        sha256 = "sha256-RTBZL8LoWyWxqfeWZEM9oJI3waJw5NeOpao6LHIp4tE=";
      };
    };
  in
    buildPythonApplication rec {
      pname = "yewtube";
      version = "2.9.0";
      doCheck = false;
      propagatedBuildInputs = [
        requests
        pyreadline
        pyperclip
        youtube-search-python
        yt-dlp
        pip
        #
        # clipboard
        #
        pkgs.xclip
        #
        # player
        #
        pkgs.mpv
        #
        # mpris
        #
        dbus-python
        pkgs.python311Packages.pygobject3
      ];
      src = fetchPypi {
        inherit pname version;
        sha256 = "sha256-YvieViyeMZbIMr3fBg27YzqaFi1eZ5zFYrnPnX/kdjM=";
      };
    }
