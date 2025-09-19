let
  pkgs = import ./nixpkgs.nix;
  orig = builtins.fetchurl {
    url = "https://raw.githubusercontent.com/bitcoin/bips/refs/heads/master/bip-0039/english.txt";
    sha256 = "1nnv4hxyv8pkxzw9yvb40f2yb47wkqckz3qdi3w4nyvjli9yspig";
  };
in
  pkgs.writeShellApplication {
    name = "bip39-wordlist";
    text = ''
      COLOR="${"$"}{1:-black}"
      echo '<!DOCTYPE html>'
      echo '<html><head><meta charset="UTF-8">'
      echo '<style>'
      echo "* { margin: 0; padding: 0; font-family: sans-serif; font-size: 7.5pt; color: $COLOR; }"
      echo '.container { column-width: 68px; column-gap: 1px; }'
      echo '.item { break-inside: avoid; white-space: nowrap; line-height: 1; }'
      echo '</style></head><body>'
      echo '<div class="container">'
      ${pkgs.busybox}/bin/awk '{printf "<div class=\"item\">%d %s</div>\n", NR, $0}' "${orig}"
      echo '</div></body></html>'
    '';
  }
