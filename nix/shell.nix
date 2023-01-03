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
    #
    # NOTE : takes too much disk space
    # and time to build, does not worth it.
    #
    # pkgs.haskell-language-server
  ];
}
