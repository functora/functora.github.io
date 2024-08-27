module Functora.Bolt11Spec (spec) where

import qualified Bitcoin.Address as Btc
import qualified Bitcoin.Address.Hash as Btc
import qualified Bitcoin.Address.Settings as Btc
import Functora.Bolt11
import Functora.Prelude
import Test.Hspec

goodSamples :: [(Text, Bolt11)]
goodSamples =
  [ ( "lnbc1pvjluezsp5zyg3zyg3zyg3zyg3zyg3zyg3zyg3zyg3zyg3zyg3zyg3zyg3zygspp5qqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqypqdpl2pkx2ctnv5sxxmmwwd5kgetjypeh2ursdae8g6twvus8g6rfwvs8qun0dfjkxaq9qrsgq357wnc5r2ueh7ck6q93dj32dlqnls087fxdwk8qakdyafkq3yap9us6v52vjjsrvywa6rt52cm9r9zqt8r2t7mlcwspyetp5h2tztugp9lfyql",
      Bolt11
        { bolt11Hrp =
            Bolt11Hrp
              { bolt11HrpNet = BitcoinMainnet,
                bolt11HrpAmt = Nothing
              },
          bolt11Timestamp = 1496314658,
          bolt11Tags =
            [ PaymentSecret "1111111111111111111111111111111111111111111111111111111111111111",
              PaymentHash "0001020304050607080900010203040506070809000102030405060708090102",
              Description "Please consider supporting this project",
              Features
                [ Feature 8 Var_onion_optin Required,
                  Feature 14 Payment_secret Required
                ]
            ],
          bolt11Signature =
            Bolt11Sig
              { bolt11SigVal =
                  "8d3ce9e28357337f62da0162d9454df827f83cfe499aeb1c1db349d4d81127425e434ca29929406c23bba1ae8ac6ca32880b38d4bf6ff874024cac34ba9625f1",
                bolt11SigRecoveryFlag = 1
              }
        }
    ),
    ( "lnbc20m1pvjluezpp5qqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqypqhp58yjmdan79s6qqdhdzgynm4zwqd5d7xmw5fk98klysy043l2ahrqsfpp3qjmp7lwpagxun9pygexvgpjdc4jdj85fr9yq20q82gphp2nflc7jtzrcazrra7wwgzxqc8u7754cdlpfrmccae92qgzqvzq2ps8pqqqqqqpqqqqq9qqqvpeuqafqxu92d8lr6fvg0r5gv0heeeqgcrqlnm6jhphu9y00rrhy4grqszsvpcgpy9qqqqqqgqqqqq7qqzqj9n4evl6mr5aj9f58zp6fyjzup6ywn3x6sk8akg5v4tgn2q8g4fhx05wf6juaxu9760yp46454gpg5mtzgerlzezqcqvjnhjh8z3g2qqdhhwkj",
      Bolt11
        { bolt11Hrp =
            Bolt11Hrp
              { bolt11HrpNet = BitcoinMainnet,
                bolt11HrpAmt =
                  Just
                    Bolt11HrpAmt
                      { bolt11HrpAmtNum = 20,
                        bolt11HrpAmtMul = Milli
                      }
              },
          bolt11Timestamp = 1496314658,
          bolt11Tags =
            [ PaymentHash "0001020304050607080900010203040506070809000102030405060708090102",
              DescriptionHash
                "3925b6f67e2c340036ed12093dd44e0368df1b6ea26c53dbe4811f58fd5db8c1",
              OnchainFallback
                . Btc.P2PKH (Btc.PrefixP2PKH 0)
                . fromMaybe (error "BADHASH")
                . Btc.parsePubHash160
                $ unHex "04b61f7dc1ea0dc99424464cc4064dc564d91e89",
              ExtraRouteInfo
                [ Route
                    { routePubKey =
                        "029e03a901b85534ff1e92c43c74431f7ce72046060fcf7a95c37e148f78c77255",
                      routeShortChanId = "0102030405060708",
                      routeFeeBaseMsat = 1,
                      routeFeePropMillionth = 20,
                      routeCltvExpiryDelta = 3
                    },
                  Route
                    { routePubKey =
                        "039e03a901b85534ff1e92c43c74431f7ce72046060fcf7a95c37e148f78c77255",
                      routeShortChanId = "030405060708090a",
                      routeFeeBaseMsat = 2,
                      routeFeePropMillionth = 30,
                      routeCltvExpiryDelta = 4
                    }
                ]
            ],
          bolt11Signature =
            Bolt11Sig
              { bolt11SigVal =
                  "91675cb3fad8e9d915343883a49242e074474e26d42c7ed914655689a8074553733e8e4ea5ce9b85f69e40d755a55014536b12323f8b220600c94ef2b9c51428",
                bolt11SigRecoveryFlag = 0
              }
        }
    )
  ]

spec :: Spec
spec = do
  it "goodSamples"
    . forM_ goodSamples
    . uncurry
    $ \lhs rhs -> decodeBolt11 lhs `shouldBe` Right rhs
