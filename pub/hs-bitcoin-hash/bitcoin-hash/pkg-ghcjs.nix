{ mkDerivation, base, bytestring, ghcjs-base, stdenv, tasty, tasty-hunit, time
, nix-gitignore }:
mkDerivation {
  pname = "bitcoin-hash";
  version = "0.1";
  src = nix-gitignore.gitignoreSourcePure ../.gitignore ./.;
  libraryHaskellDepends = [ base bytestring ghcjs-base ];
  testHaskellDepends = [ base bytestring tasty tasty-hunit time ];
  homepage = "https://gitlab.com/k0001/hs-bitcoin-hash";
  description = "Bitcoin hash primitives";
  license = stdenv.lib.licenses.asl20;
}
