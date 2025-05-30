{pkgs, ...}: let
  repo = {
    pname = "vkdoom";
    version = "83441734da61cff298ec5e0c11cf799c7bac1aac";
    src = pkgs.fetchgit {
      url = "https://github.com/dpjudas/VkDoom";
      rev = "83441734da61cff298ec5e0c11cf799c7bac1aac";
      fetchSubmodules = false;
      deepClone = false;
      leaveDotGit = true;
      sparseCheckout = [];
      sha256 = "sha256-vngGHLeAkBPOYuINqf8er6XFWTuTbfJDT3YCF/MbIQk=";
    };
    date = "2025-05-30";
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
