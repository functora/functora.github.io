module Functora.UriSpec (spec) where

import Functora.Prelude
import Functora.Uri
import Test.Hspec
import Text.URI

data Foo = Foo
  { fooBar :: Int,
    fooBuz :: Text,
    fooBuf :: String
  }
  deriving stock (Eq, Ord, Show, Data, Generic)

instance ToQuery Foo

instance FromQuery Foo

mkSample :: (MonadThrow m) => m (Foo, [QueryParam])
mkSample = do
  let bar = 123 :: Int
  let buz = "example" :: Text
  let buf = "Hello, World!" :: String
  kBar <- mkQueryKey "bar"
  vBar <- mkQueryValue $ inspect bar
  kBuz <- mkQueryKey "buz"
  vBuz <- mkQueryValue buz
  kBuf <- mkQueryKey "buf"
  vBuf <- mkQueryValue $ from @String @Text buf
  pure
    ( Foo bar buz buf,
      [ QueryParam kBar vBar,
        QueryParam kBuz vBuz,
        QueryParam kBuf vBuf
      ]
    )

spec :: Spec
spec = do
  it "ToQuery/FromQuery" $ do
    sample <- mkSample
    toQuery (fst sample) `shouldBe` snd sample
    fromQuery (snd sample) `shouldBe` Right (fst sample)
