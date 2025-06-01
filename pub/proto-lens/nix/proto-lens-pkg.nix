{ mkDerivation, base, bytestring, containers, deepseq, ghc-prim
, hpack, lens-family, ncurses, parsec, pretty, primitive
, profunctors, protoc, QuickCheck, stdenv, tagged, tasty
, tasty-quickcheck, text, transformers, vector
}:
mkDerivation {
  pname = "proto-lens";
  version = "0.7.1.0";
  src = ./../proto-lens;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    base bytestring containers deepseq ghc-prim lens-family parsec
    pretty primitive profunctors tagged text transformers vector
  ];
  librarySystemDepends = [ ncurses ];
  libraryToolDepends = [ hpack protoc ];
  testHaskellDepends = [
    base bytestring QuickCheck tasty tasty-quickcheck vector
  ];
  testSystemDepends = [ ncurses ];
  testToolDepends = [ protoc ];
  prePatch = "hpack";
  homepage = "https://github.com/google/proto-lens#readme";
  description = "A lens-based implementation of protocol buffers in Haskell";
  license = stdenv.lib.licenses.bsd3;
}
