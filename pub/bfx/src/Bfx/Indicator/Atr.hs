{-# OPTIONS_HADDOCK show-extensions #-}

module Bfx.Indicator.Atr
  ( Atr (..),
    AtrPeriod (..),
    defAtrPeriod,
    mkAtrConduit,
    mkAtr,
  )
where

import Bfx.Data.Type
import Bfx.Indicator.Tr (Tr)
import qualified Bfx.Indicator.Tr as Tr
import Conduit ((.|))
import qualified Conduit as C
import qualified Data.Map as Map
import qualified Data.Vector as V
import Functora.Money
import Functora.Prelude
import qualified Prelude

newtype Atr = Atr
  { unAtr :: QuotePerBase
  }
  deriving stock
    ( Eq,
      Ord,
      Show,
      Read,
      Data,
      Generic
    )

newtype AtrPeriod = AtrPeriod
  { unAtrPeriod :: Natural
  }
  deriving stock
    ( Eq,
      Ord,
      Show,
      Read,
      Data,
      Generic
    )

defAtrPeriod :: AtrPeriod
defAtrPeriod = AtrPeriod 14

mkAtrConduit ::
  ( Monad m
  ) =>
  (a -> Candle) ->
  AtrPeriod ->
  C.ConduitT a (a, Atr) m ()
mkAtrConduit mkCandle per = do
  Tr.mkTrConduit mkCandle
    .| C.slidingWindowC period
    .| ( whileM $ do
          mtrs <- fmap (>>= nonEmpty) C.await
          case mtrs of
            Just trs | length trs == period -> do
              C.yield
                ( fst $ last trs,
                  Atr
                    . QuotePerBase
                    $ (sum $ unQuotePerBase . Tr.unTr . snd <$> trs)
                    / from @Natural @(Ratio Natural)
                      (unsafeFrom @Int @Natural period)
                )
              pure True
            _ ->
              pure False
       )
  where
    period =
      case unsafeFrom @Natural @Int $ unAtrPeriod per of
        x | x < 1 -> error $ "Bad ATR period " <> inspect period
        x -> x

mkAtr :: AtrPeriod -> NonEmpty Candle -> Map UTCTime Atr
mkAtr period cs =
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
    trs = Tr.mkTrs cs
    intPeriod = unsafeFrom @Natural @Int $ unAtrPeriod period
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
