let
  sources = import ./sources.nix {};
in
  (import sources.flake-compat {
    src = sources.nixpak;
  })
  .defaultNix
