module Functora.Bolt11Spec (spec) where

import Functora.Bolt11
import Functora.Prelude
import Test.Hspec

goodSamples :: [(Text, Bolt11)]
goodSamples =
  [ ( "lnbc1pvjluezsp5zyg3zyg3zyg3zyg3zyg3zyg3zyg3zyg3zyg3zyg3zyg3zyg3zygspp5qqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqypqdpl2pkx2ctnv5sxxmmwwd5kgetjypeh2ursdae8g6twvus8g6rfwvs8qun0dfjkxaq9qrsgq357wnc5r2ueh7ck6q93dj32dlqnls087fxdwk8qakdyafkq3yap9us6v52vjjsrvywa6rt52cm9r9zqt8r2t7mlcwspyetp5h2tztugp9lfyql",
      Bolt11
        { bolt11HRP =
            Bolt11HRP
              { bolt11Currency = Bitcoin,
                bolt11Amount = Nothing
              },
          bolt11Timestamp = 1496314658,
          bolt11Tags =
            [ PaymentSecret "1111111111111111111111111111111111111111111111111111111111111111",
              PaymentHash "0001020304050607080900010203040506070809000102030405060708090102",
              Description "Please consider supporting this project",
              FeatureBits [16, 8, 0]
            ],
          bolt11Signature =
            "8d3ce9e28357337f62da0162d9454df827f83cfe499aeb1c1db349d4d81127425e434ca29929406c23bba1ae8ac6ca32880b38d4bf6ff874024cac34ba9625f101"
        }
    )
  ]

spec :: Spec
spec = do
  it "goodSamples" . forM_ goodSamples . uncurry $
    \lhs rhs -> decodeBolt11 lhs `shouldBe` Right rhs
