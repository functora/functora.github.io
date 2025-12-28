let
  pkgs = import ./nixpkgs.nix;
  vsn = "bak-v0.1.0";
in {
  bak-status = pkgs.writeShellApplication {
    name = "bak-status";
    text = ''
      cd ${toString ../.}/bak
      sha256sum -c checksums.txt 2>&1 | grep -v ': OK$' || true
      find . -type f ! -name checksums.txt -exec sh -c 'f="''${1#./}"; grep -q "$f" checksums.txt || echo "$1 NEW"' sh {} \;
      echo "bak-status ==> completed ${vsn}"
    '';
  };
  bak-commit = pkgs.writeShellApplication {
    name = "bak-commit";
    text = ''
      cd ${toString ../.}/bak
      find . -type f ! -name 'checksums.txt' \
        -exec sha256sum {} \; > checksums.txt
      echo "bak-commit ==> completed ${vsn}"
    '';
  };
  bak-encrypt = pkgs.writeShellApplication {
    name = "bak-encrypt";
    text = ''
      cd ${toString ../.}
      echo "bak-encrypt ==> starting ${vsn}"
      tar -czf ./out/${vsn}.tar.gz ./bak
      echo "bak-encrypt ==> encrypting ${vsn}"
      ${pkgs.age}/bin/age \
        --encrypt \
        --passphrase \
        --output ./out/${vsn}.tar.gz.age \
        ./out/${vsn}.tar.gz
      echo "bak-encrypt ==> completed ${vsn}"
    '';
  };
  bak-decrypt = pkgs.writeShellApplication {
    name = "bak-decrypt";
    text = ''
      cd ${toString ../.}
      ${pkgs.age}/bin/age \
        --decrypt \
        --output ./out/${vsn}.tar.gz \
        ./out/${vsn}.tar.gz.age
      tar -xzf ./out/${vsn}.tar.gz ./
      echo "bak-decrypt ==> completed ${vsn}"
    '';
  };
}
