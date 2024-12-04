{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module Bfx.Class.ToRequestParam
  ( ToRequestParam (..),
    SomeQueryParam (..),
    unQueryParam,
  )
where

import Bfx.Data.Web
import Bfx.Import.External
import qualified Data.ByteString as BS
import qualified Data.Text as T
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

instance ToRequestParam Natural where
  toTextParam =
    T.pack . Prelude.show

instance ToRequestParam UTCTime where
  toTextParam =
    toTextParam . utcTimeToMicros

instance ToRequestParam Text where
  toTextParam =
    id

instance ToRequestParam (BuyOrSell, BaseOrQuote, MoneyAmount) where
  toTextParam (bos, boq, MoneyAmount amt) =
    if bos == Sell && boq == Base
      then toTextParam $ (-1) * rat amt
      else toTextParam $ rat amt
    where
      rat = abs . from @(Ratio Natural) @Rational

instance ToRequestParam QuotePerBase where
  toTextParam =
    toTextParam
      . abs
      . from @(Ratio Natural) @Rational
      . unQuotePerBase
