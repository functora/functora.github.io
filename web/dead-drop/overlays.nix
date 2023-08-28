{functora}: [
  (
    _: pkgs: let
      pkcs7 = {
        mkDerivation,
        base,
        bytestring,
        Cabal,
        HUnit,
        lib,
        QuickCheck,
      }:
        mkDerivation {
          pname = "pkcs7";
          version = "1.0.0.1";
          sha256 = "0i4hpy9rmc49apsmadz72prgmkb13ww575v8flhdymd3dkjn6b52";
          libraryHaskellDepends = [base bytestring];
          testHaskellDepends = [base bytestring Cabal HUnit QuickCheck];
          homepage = "https://github.com/kisom/pkcs7";
          description = "PKCS #7 padding in Haskell";
          license = lib.licenses.mit;
        };
      cryptohash-sha256 = {
        mkDerivation,
        base,
        base16-bytestring,
        bytestring,
        criterion,
        lib,
        SHA,
        tasty,
        tasty-hunit,
        tasty-quickcheck,
      }:
        mkDerivation {
          pname = "cryptohash-sha256";
          version = "0.11.102.1";
          sha256 = "1xkb7iqplbw4fy1122p79xf1zcb7k44rl0wmfj1q06l7cdqxr9vk";
          revision = "2";
          editedCabalFile = "1g7lpcn3zhrp4a65gj71v2164387r0m42pyavlx6bbifhyar1kkj";
          configureFlags = ["-fuse-cbits"];
          isLibrary = true;
          isExecutable = true;
          libraryHaskellDepends = [base bytestring];
          testHaskellDepends = [
            base
            base16-bytestring
            bytestring
            SHA
            tasty
            tasty-hunit
            tasty-quickcheck
          ];
          benchmarkHaskellDepends = [base bytestring criterion SHA];
          homepage = "https://github.com/hvr/cryptohash-sha256";
          description = "Fast, pure and practical SHA-256 implementation";
          license = lib.licenses.bsd3;
        };
      qrcode-core = {
        mkDerivation,
        base,
        binary,
        bytestring,
        case-insensitive,
        containers,
        dlist,
        lib,
        primitive,
        text,
        vector,
      }:
        mkDerivation {
          pname = "qrcode-core";
          version = "0.9.8";
          sha256 = "1f8ydz1s07p5817l746vxvsh8xprbkrffhmgv2wkqx61s05rc5ch";
          libraryHaskellDepends = [
            base
            binary
            bytestring
            case-insensitive
            containers
            dlist
            primitive
            text
            vector
          ];
          homepage = "https://github.com/alexkazik/qrcode#readme";
          description = "QR code library in pure Haskell";
          license = lib.licenses.mit;
        };
      witch-mini = {
        mkDerivation,
        base,
        bytestring,
        containers,
        fetchgit,
        HUnit,
        lib,
        tagged,
        text,
        transformers,
      }:
        mkDerivation {
          pname = "witch-mini";
          version = "1.2.0.2";
          src = fetchgit {
            url = "https://github.com/functora/functora.github.io.git";
            sha256 = "0jcf9svy1hwryfn3iy41kawxkvnky2zk04p7h7n37a70nj94dvk1";
            rev = "9f1a9179b140896b74703d4dce9599acf494af39";
            fetchSubmodules = false;
          };
          postUnpack = "sourceRoot+=/pub/witch-mini; echo source root reset to $sourceRoot";
          libraryHaskellDepends = [base bytestring containers tagged text];
          testHaskellDepends = [
            base
            bytestring
            containers
            HUnit
            tagged
            text
            transformers
          ];
          description = "Convert values from one type into another";
          license = lib.licenses.mit;
        };
      HUnit = {
        mkDerivation,
        base,
        call-stack,
        deepseq,
        filepath,
        lib,
      }:
        mkDerivation {
          pname = "HUnit";
          version = "1.6.2.0";
          sha256 = "1as4sw5y39c3zrmr6sb8zbw74c9gdn4401y0dx45ih7zf6457dxh";
          libraryHaskellDepends = [base call-stack deepseq];
          testHaskellDepends = [base call-stack deepseq filepath];
          homepage = "https://github.com/hspec/HUnit#readme";
          description = "A unit testing framework for Haskell";
          license = lib.licenses.bsd3;
        };
      universum = {
        mkDerivation,
        base,
        bytestring,
        containers,
        deepseq,
        doctest,
        gauge,
        ghc-prim,
        Glob,
        hashable,
        hedgehog,
        lib,
        microlens,
        microlens-mtl,
        mtl,
        safe-exceptions,
        stm,
        tasty,
        tasty-hedgehog,
        text,
        transformers,
        unordered-containers,
        utf8-string,
        vector,
      }:
        mkDerivation {
          pname = "universum";
          version = "1.7.2";
          sha256 = "1ka7q5vr9xkf8z5mzpkp648mpf8az7b14lnhbvfakg3v5xy3f7gb";
          revision = "1";
          editedCabalFile = "17w3zpbv5ap9as506fn43xlnh6sqxni8mmczlp5l86hvn7zd8y6z";
          libraryHaskellDepends = [
            base
            bytestring
            containers
            deepseq
            ghc-prim
            hashable
            microlens
            microlens-mtl
            mtl
            safe-exceptions
            stm
            text
            transformers
            unordered-containers
            utf8-string
            vector
          ];
          testHaskellDepends = [
            base
            bytestring
            doctest
            Glob
            hedgehog
            tasty
            tasty-hedgehog
            text
          ];
          benchmarkHaskellDepends = [
            base
            containers
            gauge
            text
            unordered-containers
          ];
          homepage = "https://github.com/serokell/universum";
          description = "Custom prelude used in Serokell";
          license = lib.licenses.mit;
        };
      functora-acme = {
        mkDerivation,
        base,
        base16-bytestring,
        base64-bytestring,
        bmp,
        bytestring,
        Crypto,
        cryptohash-sha256,
        fetchgit,
        lens,
        lib,
        pkcs7,
        qrcode-core,
        syb,
        tagged,
        text,
        universum,
        vector,
        witch-mini,
      }:
        mkDerivation {
          pname = "functora-acme";
          version = "0.1.0.0";
          src = fetchgit {
            url = "https://github.com/functora/functora.github.io.git";
            sha256 = "1ragx7p99s2i48r4frrgwxvjdkpl81vg0vrnwy6iqf3qfw1cbr5l";
            rev = "b88c9915705f4387e6ee44936068cb2acc2efefd";
            fetchSubmodules = false;
          };
          postUnpack = "sourceRoot+=/pub/functora/src; echo source root reset to $sourceRoot";
          libraryHaskellDepends = [
            base
            base16-bytestring
            base64-bytestring
            bmp
            bytestring
            Crypto
            cryptohash-sha256
            lens
            pkcs7
            qrcode-core
            syb
            tagged
            text
            universum
            vector
            witch-mini
          ];
          homepage = "https://github.com/functora/functora.github.io/tree/master/pub/functora";
          description = "Custom prelude";
          license = lib.licenses.bsd3;
        };
      extendHaskell = ghcpkgs:
        ghcpkgs.extend (
          self: super: {
            pkcs7 = pkgs.haskell.lib.doJailbreak (
              pkgs.lib.callPackageWith
              self
              pkcs7 {lib = pkgs.lib;}
            );
            cryptohash-sha256 = pkgs.haskell.lib.doJailbreak (
              pkgs.lib.callPackageWith
              self
              cryptohash-sha256 {lib = pkgs.lib;}
            );
            qrcode-core = pkgs.haskell.lib.doJailbreak (
              pkgs.lib.callPackageWith
              self
              qrcode-core {lib = pkgs.lib;}
            );
            HUnit = pkgs.haskell.lib.doJailbreak (
              pkgs.lib.callPackageWith
              self
              HUnit {lib = pkgs.lib;}
            );
            witch-mini = pkgs.haskell.lib.doJailbreak (
              pkgs.lib.callPackageWith
              self
              witch-mini {
                lib = pkgs.lib;
                fetchgit = pkgs.fetchgit;
              }
            );
            universum = pkgs.haskell.lib.doJailbreak (
              pkgs.lib.callPackageWith
              self
              universum {lib = pkgs.lib;}
            );
            functora-acme = pkgs.haskell.lib.doJailbreak (
              pkgs.lib.callPackageWith
              self
              functora-acme {
                lib = pkgs.lib;
                fetchgit = pkgs.fetchgit;
              }
            );
          }
        );
    in {
      haskell =
        pkgs.haskell
        // {
          packages =
            pkgs.haskell.packages
            // {
              ghc865 = extendHaskell pkgs.haskell.packages.ghc865;
              ghcjs86 = extendHaskell pkgs.haskell.packages.ghcjs86;
            };
        };
    }
  )
]
