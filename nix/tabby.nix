let
  pkgs = import ./nixpkgs.nix;
  pname = "tabby-vulkan";
  version = "0.25.1";
  src = builtins.fetchurl {
    url = "https://github.com/TabbyML/tabby/releases/download/v${version}/tabby_x86_64-manylinux_2_28-vulkan.tar.gz";
    sha256 = "1hfddjpa9q42giya5zxqw9db0vhg0pqgd4hy4yfipcw8diz169i7";
  };
in
  pkgs.stdenv.mkDerivation {
    inherit src pname version;
    buildInputs = [
      # tabby
      pkgs.libz
      pkgs.libgcc
      # llama-server
      pkgs.vulkan-loader # libvulkan
      (pkgs.lib.getLib pkgs.stdenv.cc.cc) # libgomp libstdc++
    ];
    nativeBuildInputs = [
      pkgs.autoPatchelfHook
    ];
    installPhase = ''
      mkdir -p $out/bin
      cp tabby $out/bin
      cp llama-server $out/bin
    '';
    fixupPhase = ''
      autoPatchelf $out/bin
    '';
  }
