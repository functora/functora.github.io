{-# OPTIONS_HADDOCK show-extensions #-}

module Bfx.Indicator.Atr
  ( Atr (..),
    atr,
  )
where

import Bfx.Data.Type
import Bfx.Indicator.Tr (Tr)
import qualified Bfx.Indicator.Tr as Tr
import qualified Data.Map as Map
import qualified Data.Vector as V
import Functora.Money
import Functora.Prelude
import qualified Prelude

newtype Atr = Atr
  { unAtr :: QuotePerBase
  }
  deriving newtype
    ( Eq,
      Ord
    )
  deriving stock
    ( Data,
      Generic
    )

newtype AtrPeriod = AtrPeriod
  { unAtrPeriod :: Natural
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

stdAtrPeriod :: AtrPeriod
stdAtrPeriod =
  AtrPeriod 14

atr :: NonEmpty Candle -> Map UTCTime Atr
atr =
  atr0 stdAtrPeriod

atr0 :: AtrPeriod -> NonEmpty Candle -> Map UTCTime Atr
atr0 period cs =
  if stopAtIdx < 0 || intPeriod < 1
    then mempty
    else
      unsafeAtr
        intPeriod
        trs
        stopAtIdx
        0
        mempty
  where
    trs = Tr.tr cs
    intPeriod = Prelude.fromIntegral period
    stopAtIdx = length trs - intPeriod

unsafeAtr ::
  Int ->
  Vector (UTCTime, Tr) ->
  Int ->
  Int ->
  Map UTCTime Atr ->
  Map UTCTime Atr
unsafeAtr intPeriod xs stopAtIdx currentIdx acc =
  if stopAtIdx < currentIdx
    then acc
    else
      unsafeAtr intPeriod xs stopAtIdx (currentIdx + 1)
        $ Map.insert time val acc
  where
    chunk = V.slice currentIdx intPeriod xs
    time = fst $ V.last chunk
    val =
      Atr
        . QuotePerBase
        . (/ Prelude.fromIntegral intPeriod)
        . V.foldl1 (+)
        $ V.map (unQuotePerBase . Tr.unTr . snd) chunk
