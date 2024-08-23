{ mkDerivation, base, base16-bytestring, bytestring, hedgehog
, secp256k1-haskell, stdenv, tasty, tasty-hedgehog, tasty-hunit
, nix-gitignore
}:
mkDerivation {
  pname = "bitcoin-keys";
  version = "0.1";
  src = nix-gitignore.gitignoreSourcePure ../.gitignore ./.;
  libraryHaskellDepends = [ base bytestring secp256k1-haskell ];
  testHaskellDepends = [
    base base16-bytestring bytestring hedgehog tasty tasty-hedgehog
    tasty-hunit
  ];
  homepage = "https://gitlab.com/k0001/hs-bitcoin-keys";
  description = "Bitcoin keys";
  license = stdenv.lib.licenses.asl20;
}
