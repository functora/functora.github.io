{ mkDerivation, base, bytestring, containers, filepath, ghc
, ghc-paths, ghc-source-gen, hpack, lens-family, ncurses, pretty
, proto-lens, proto-lens-runtime, protobuf, protoc, stdenv, text
}:
mkDerivation {
  pname = "proto-lens-protoc";
  version = "0.7.1.0";
  src = ./../proto-lens-protoc;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base filepath ];
  librarySystemDepends = [ ncurses ];
  libraryToolDepends = [ hpack protobuf protoc ];
  executableHaskellDepends = [
    base bytestring containers filepath ghc ghc-paths ghc-source-gen
    lens-family pretty proto-lens proto-lens-runtime text
  ];
  executableSystemDepends = [ ncurses ];
  executableToolDepends = [ protoc ];
  prePatch = "hpack";
  homepage = "https://github.com/google/proto-lens#readme";
  description = "Protocol buffer compiler for the proto-lens library";
  license = stdenv.lib.licenses.bsd3;
}
