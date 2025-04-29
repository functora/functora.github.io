let
  pkgs = import ./nixpkgs.nix;
  prev = "bak-v0.1.0.0";
  next = "bak-v0.1.0.0";
  # prev-derivation = builtins.fetchurl {
  #   url = "https://github.com/functora/functora.github.io/releases/download/${prev}/${prev}.tar.gz.age";
  #   sha256 = "";
  # };
in {
  bak-status = pkgs.writeShellApplication {
    name = "bak-status";
    text = ''
      cd ${toString ../.}/bak
      sha256sum -c checksums.txt 2>&1 | grep -v ': OK$' || true
      find . -type f ! -name checksums.txt -exec sh -c 'f="''${1#./}"; grep -q "$f" checksums.txt || echo "$1 NEW"' sh {} \;
      echo "bak-status ==> completed ${next}"
    '';
  };
  bak-commit = pkgs.writeShellApplication {
    name = "bak-commit";
    text = ''
      cd ${toString ../.}/bak
      find . -type f ! -name 'checksums.txt' \
        -exec sha256sum {} \; > checksums.txt
      echo "bak-commit ==> completed ${next}"
    '';
  };
  bak-encrypt = pkgs.writeShellApplication {
    name = "bak-encrypt";
    text = ''
      cd ${toString ../.}
      tar -czf ./out/${next}.tar.gz ./bak
      ${pkgs.age}/bin/age \
        --encrypt \
        --passphrase \
        --output ./out/${next}.tar.gz.age \
        ./out/${next}.tar.gz
      echo "bak-encrypt ==> completed ${next}"
    '';
  };
  # bak-decrypt = pkgs.writeShellApplication {
  #   name = "bak-decrypt";
  #   text = ''
  #     cd ${toString ../.}
  #     ${pkgs.age}/bin/age \
  #       --decrypt \
  #       --output ./out/${pkg}.tar.gz \
  #       ${prev-derivation}
  #     tar -xzf ./out/${pkg}.tar.gz ./
  #   '';
  # };
}
