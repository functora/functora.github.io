{ pkgs ? import <nixpkgs> {} }:
with pkgs.python3Packages;
let
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
    pkgs.mpv
  ];
  src = fetchPypi {
    inherit pname version;
    sha256 = "sha256-YvieViyeMZbIMr3fBg27YzqaFi1eZ5zFYrnPnX/kdjM=";
  };
}
