{ stdenv, lib, fetchurl, SDL2, libx11, libjpeg, libpng, zlib, libvorbis, libogg, libtheora, curl, libGL }:

stdenv.mkDerivation rec {
  pname = "zircon";
  version = "release-79";
  rev = "7729a284ec81c20a6f30d4071f48728b4a56c980";

  src = fetchurl {
    url = "https://github.com/FriskTheFallenHuman/zircon-engine/archive/${rev}.tar.gz";
    sha256 = "77ae90d8b55c1995c4c338186133ba200a2557f0a921b0a1a26bcebdec13667e";
  };

  buildInputs = [ SDL2.out SDL2.dev libx11 libjpeg libpng zlib libvorbis libogg libtheora curl libGL ];
  hardeningDisable = [ "format" ];

  doCheck = false;

  buildPhase = ''
    sed -i 's/-Werror=c++-compat/-Wno-error=c++-compat -Wno-implicit-function-declaration -Wno-int-conversion/' makefile
    make sdl-release \
      SDL_CONFIG=${SDL2.dev}/bin/sdl2-config
  '';

  installPhase = ''
    mkdir -p $out/bin
    install -m755 zircon_beta_linux_sdl $out/bin/zircon
  '';

  meta = with lib; {
    homepage = "https://www.moddb.com/mods/zircon-engine";
    description = "Zircon Engine - a DarkPlaces Mark V Quake engine";
    license = licenses.gpl2;
    platforms = platforms.linux;
  };
}
