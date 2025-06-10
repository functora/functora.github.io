let
  pkgs = import ./nixpkgs.nix;
  pips = pkgs.fetchgit {
    url = "https://huggingface.co/rhasspy/piper-voices";
    rev = "e1686869b0722bd873ee3154fe19d32e5f593322";
    sha256 = "sha256-sldoRdODmy1HAfA2y0ozonjq3s63IZSvpkKoMHhXvO0=";
    fetchLFS = true;
  };
  # et = pkgs.fetchgit {
  #   url = "https://huggingface.co/csukuangfj/vits-coqui-et-cv";
  #   rev = "2e5249ce7a605279a5cb9cf70cc6df7ab801a55a";
  #   sha256 = "sha256-GdsxCmoZ1PUqL8QeOCey9XCpXMAW7U41xt5x0h/fIgA=";
  #   fetchLFS = true;
  # };
in
  pkgs.stdenv.mkDerivation {
    name = "piper-voices-full";
    dontBuild = true;
    dontUnpack = true;
    installPhase = ''
      mkdir -p $out
      cp -R ${pips}/* $out
    '';
    # mkdir -p $out/et/vits-coqui-et-cv
    # cp ${et}/model.onnx $out/et/vits-coqui-et-cv/model.onnx
    # ${pkgs.gnused}/bin/sed \
    #   -E 's/\b-?Infinity\b/null/g; s/\bNaN\b/null/g' \
    #   ${et}/config.json > \
    #   $out/et/vits-coqui-et-cv/model.onnx.json
  }
