with (import ./project.nix);
shellFor {
  exactDeps = true;
  withHoogle = false;
  buildInputs = [
    pkgs.cabal-install
    pkgs.hpack
    pkgs.hlint
    pkgs.ghcid
  ];
}
