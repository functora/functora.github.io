{ mkDerivation, base, bytestring, containers, deepseq, filepath
, hpack, lens-family, ncurses, proto-lens, protoc, stdenv, text
, vector
}:
mkDerivation {
  pname = "proto-lens-runtime";
  version = "0.7.0.1";
  src = ./../proto-lens-runtime;
  libraryHaskellDepends = [
    base bytestring containers deepseq filepath lens-family proto-lens
    text vector
  ];
  librarySystemDepends = [ ncurses ];
  libraryToolDepends = [ hpack protoc ];
  doHaddock = false;
  prePatch = "hpack";
  homepage = "https://github.com/google/proto-lens#readme";
  license = stdenv.lib.licenses.bsd3;
}
