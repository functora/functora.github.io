let
  pkgs = import ./nixpkgs.nix;
in
  pkgs.fetchgit {
    url = "https://huggingface.co/rhasspy/piper-voices";
    rev = "e1686869b0722bd873ee3154fe19d32e5f593322";
    sha256 = "sha256-sldoRdODmy1HAfA2y0ozonjq3s63IZSvpkKoMHhXvO0=";
    fetchLFS = true;
  }
