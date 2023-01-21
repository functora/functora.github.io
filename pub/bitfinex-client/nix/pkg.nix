{ mkDerivation, aeson, base, base16-bytestring, bytestring
, combinat, containers, cryptonite, envparse, extra
, generic-pretty-instances, GenericPretty, gnuplot, hpack, hspec
, http-client, http-client-tls, http-types, katip, lens-aeson, lib
, memory, parallel, persistent, pretty, reanimate, siggy-chardust
, singletons, singletons-base, table-layout, template-haskell
, temporary, text, time, transformers, units, universum, unliftio
, vector, witch
}:
mkDerivation {
  pname = "bitfinex-client";
  version = "0.1.0.0";
  src = ./..;
  libraryHaskellDepends = [
    aeson base base16-bytestring bytestring combinat containers
    cryptonite envparse extra generic-pretty-instances GenericPretty
    gnuplot http-client http-client-tls http-types katip lens-aeson
    memory parallel persistent pretty reanimate siggy-chardust
    singletons singletons-base table-layout template-haskell temporary
    text time transformers units universum unliftio vector witch
  ];
  libraryToolDepends = [ hpack ];
  testHaskellDepends = [ aeson base containers hspec ];
  prePatch = "hpack";
  homepage = "https://github.com/21it/bitfinex-client#readme";
  license = lib.licenses.bsd3;
}
