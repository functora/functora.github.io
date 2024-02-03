let
  functora = builtins.fetchTarball {
    url = "https://github.com/functora/functora.github.io/archive/f1482c538f5a97c2a440d948f45a25953d6c82d1.tar.gz";
    sha256 = "0ij4gnzqr7hsafsr3pqnqihnqlq07hs9pjbgvgmvbas47jhnhdfn";
  };
  functora-miso = import "${functora}/ghcjs/miso/default.nix" {
    overlays = import ./overlays.nix {
      inherit functora;
    };
  };
  functora-pkgs = import "${functora}/nix/nixpkgs.nix";
in
  with functora-miso; {
    inherit pkgs functora;
    dev = pkgs.haskell.packages.ghc865.callCabal2nix "app" ./. {
      miso = miso-jsaddle;
    };
    app = pkgs.haskell.packages.ghcjs.callCabal2nix "app" ./. {};
    repo = builtins.toString ./.;
    release = app.overrideAttrs (_: rec {
      postInstall = ''
        mkdir -p $out/bin/app.jsexe/static
        cp -R ${./static}/* $out/bin/app.jsexe/static
        cp ${./static}/favicon.ico $out/bin/app.jsexe/favicon.ico
        rm $out/bin/app.jsexe/static/readme.html || true
        cp ${readmeDer}/readme.html $out/bin/app.jsexe/static/
      '';
    });
    publish = functora-pkgs.writeShellApplication rec {
      name = "publish";
      text = let
        vsn = app.passthru.version;
      in ''
        (
          cd ${repo}
          nix-build -A publishDer
          mkdir -p ./docs/${vsn}
          cp -RL ./result/${vsn}/* ./docs/${vsn}
          cp -RLf ./result/${vsn}/favicon.ico ./docs/favicon.ico
          cp -RLf ./result/index.html ./docs/index.html
        )
      '';
    };
    publishDer = functora-pkgs.stdenv.mkDerivation {
      name = "publishDer";
      src = ./docs;
      dontBuild = true;
      installPhase = let
        vsn = app.passthru.version;
      in ''
        mkdir -p $out
        cp -R ./* $out || true
        if [ -d "$out/${vsn}" ]; then
          echo "Version ${vsn} does already exit!"
          exit 1
        else
          mkdir -p $out/${vsn}
          cp -R ${release}/bin/app.jsexe/* $out/${vsn}
          echo '<!doctype html><html><head><meta http-equiv="Refresh" content="0; url=${vsn}/index.html"></head><body></body></html>' > $out/index.html
          echo "Version ${vsn} has been published!"
        fi
      '';
    };
    readmeDer = functora-pkgs.stdenv.mkDerivation {
      name = "readmeDer";
      src = ./README.md;
      dontUnpack = true;
      buildPhase = ''
        ${functora-pkgs.pandoc}/bin/pandoc \
          --standalone \
          --metadata title="Functora" \
          $src > readme.html
      '';
      installPhase = ''
        mkdir -p $out
        cp ./readme.html $out/readme.html
      '';
    };
    readmeGen = functora-pkgs.writeShellApplication rec {
      name = "readmeGen";
      text = ''
        ${functora-pkgs.pandoc}/bin/pandoc \
          --standalone \
          --metadata title="Functora" \
          ${repo}/README.md > ${repo}/static/readme.html
      '';
    };
  }
