{-# OPTIONS_HADDOCK show-extensions #-}

module BitfinexClient.Indicator.Ma
  ( Ma (..),
    MaPeriod (..),
    ma,
  )
where

import BitfinexClient.Data.Metro
import BitfinexClient.Import
import qualified Data.Map as Map
import qualified Data.Vector as V

newtype Ma = Ma
  { unMa :: QuotePerBase'
  }
  deriving newtype
    ( Eq,
      Ord
    )
  deriving stock
    ( Generic
    )

instance NFData Ma

newtype MaPeriod = MaPeriod
  { unMaPeriod :: Natural
  }
  deriving newtype
    ( Eq,
      Ord,
      Show,
      Num,
      Real,
      Enum,
      Integral
    )
  deriving stock
    ( Generic
    )

instance NFData MaPeriod

ma :: MaPeriod -> NonEmpty Candle -> Map UTCTime Ma
ma period candles =
  if stopAtIdx < 0 || maPeriod < 1
    then mempty
    else
      unsafeMa
        maPeriod
        (V.fromList $ from candles)
        stopAtIdx
        0
        mempty
  where
    maPeriod = fromIntegral period
    stopAtIdx = length candles - maPeriod

unsafeMa ::
  Int ->
  Vector Candle ->
  Int ->
  Int ->
  Map UTCTime Ma ->
  Map UTCTime Ma
unsafeMa maPeriod candles stopAtIdx currentIdx acc =
  if stopAtIdx < currentIdx
    then acc
    else
      unsafeMa maPeriod candles stopAtIdx (currentIdx + 1) $
        Map.insert maUtc maVal acc
  where
    chunk = V.slice currentIdx maPeriod candles
    maUtc = candleAt $ V.last chunk
    maVal =
      Ma
        . (|/ fromIntegral maPeriod)
        . V.foldl1 (|+|)
        $ V.map (unQuotePerBase . candleClose) chunk
