{-# OPTIONS_HADDOCK show-extensions #-}

module Bfx.Class.ToRequestParam
  ( ToRequestParam (..),
    SomeQueryParam (..),
    unQueryParam,
  )
where

import Bfx.Data.Web
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Functora.Money
import Functora.Prelude
import qualified Prelude

class ToRequestParam a where
  toTextParam :: a -> Text
  toQueryParam :: a -> Maybe BS.ByteString
  toQueryParam = Just . encodeUtf8 . toTextParam

data SomeQueryParam
  = forall a.
    ( ToRequestParam a
    ) =>
    SomeQueryParam BS.ByteString a

unQueryParam ::
  SomeQueryParam ->
  (BS.ByteString, Maybe BS.ByteString)
unQueryParam (SomeQueryParam name x) =
  (name, toQueryParam x)

data E8

instance HasResolution E8 where
  resolution = const 100000000

instance ToRequestParam Rational where
  toTextParam x =
    T.pack
      $ showFixed True (fromRational x :: Fixed E8)

instance ToRequestParam Fix where
  toTextParam = inspectFix

instance ToRequestParam FixNonNeg where
  toTextParam = inspectFix

instance ToRequestParam Natural where
  toTextParam =
    T.pack . Prelude.show

instance ToRequestParam UTCTime where
  toTextParam =
    toTextParam . (`div` 1000) . utcTimeToMicros

instance ToRequestParam Text where
  toTextParam =
    id

instance ToRequestParam (BuyOrSell, MoneyAmount) where
  toTextParam (bos, amt) =
    if bos == Sell
      then toTextParam $ (-1) * unAmt amt
      else toTextParam $ unAmt amt
    where
      unAmt = abs . unFixNonNeg . unMoneyAmount

instance ToRequestParam QuotePerBase where
  toTextParam =
    toTextParam
      . abs
      . unFixNonNeg
      . unQuotePerBase

instance ToRequestParam AscOrDesc where
  toTextParam = \case
    Asc -> "1"
    Desc -> "-1"
