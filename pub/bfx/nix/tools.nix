with (import ./../../../nix/misc.nix); [
  (mkGhcid "bfx" "lib" null)
  (mkGhcid "bfx" "test" null)
]
