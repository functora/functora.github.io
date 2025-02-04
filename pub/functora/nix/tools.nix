with (import ./../../../nix/misc.nix); [
  (mkGhcidV2 {
    pkg = "functora";
    sub = "test";
  })
  (mkGhcidV2 {
    pkg = "functora";
    sub = "card-lib";
  })
  (mkGhcidV2 {
    pkg = "functora";
    sub = "card";
    opt = "-f ${toString ../cfg/card.toml}";
  })
]
