{pkgs, ...}: let
  repo = {
    pname = "vkdoom";
    version = "640a2b2c4104d957309ffe6d11de8b7e62e1cbb6";
    src = pkgs.fetchgit {
      url = "https://github.com/dpjudas/VkDoom";
      rev = "640a2b2c4104d957309ffe6d11de8b7e62e1cbb6";
      fetchSubmodules = false;
      deepClone = false;
      leaveDotGit = true;
      sparseCheckout = [];
      sha256 = "sha256-ahHtgOyB/Z+8J4Uou1CWEg1gR5jUUayoWnb3N1ahRpg=";
    };
    date = "2025-02-22";
  };
in
  pkgs.stdenv.mkDerivation {
    pname = repo.pname;
    src = repo.src;
    version = repo.date;

    nativeBuildInputs = with pkgs; [
      cmake
      git
      makeWrapper
    ];

    buildInputs = with pkgs; [
      SDL2
      gtk3
      libvpx
      openal
      zmusic
    ];

    postInstall = ''
      for bin in vkdoom vktool; do
        mv $out/bin/$bin $out/share/games/doom/$bin
        makeWrapper $out/share/games/doom/$bin $out/bin/$bin \
          --set LD_LIBRARY_PATH ${with pkgs; lib.makeLibraryPath [vulkan-loader]}
      done
    '';

    cmakeFlags = [
      "-DDYN_GTK=OFF"
      "-DDYN_OPENAL=OFF"
    ];

    NIX_CFLAGS_COMPILE = ["-Wno-error=format-security"];

    meta = with pkgs.lib; {
      description = "ZDoom based source port with a primary focus on Vulkan and modern computers";
      homepage = "https://vkdoom.org/";
      license = licenses.gpl3Plus;
      platforms = platforms.linux;
    };
  }
