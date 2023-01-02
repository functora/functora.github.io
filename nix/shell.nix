with (import ./project.nix);
let nixpkgs = import ./nixpkgs.nix;
in
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
