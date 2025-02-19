{-# OPTIONS_HADDOCK show-extensions #-}

module Bfx.Indicator.Ma
  ( Ma (..),
    MaPeriod (..),
    defMaPeriod,
    mkMaConduit,
    mkMa,
  )
where

import Bfx.Data.Type
import Conduit ((.|))
import qualified Conduit as C
import qualified Data.Map as Map
import qualified Data.Vector as V
import Functora.Money
import Functora.Prelude
import qualified Prelude

newtype Ma = Ma
  { unMa :: QuotePerBase
  }
  deriving stock
    ( Eq,
      Ord,
      Show,
      Read,
      Data,
      Generic
    )

newtype MaPeriod = MaPeriod
  { unMaPeriod :: Natural
  }
  deriving stock
    ( Eq,
      Ord,
      Show,
      Read,
      Data,
      Generic
    )

defMaPeriod :: MaPeriod
defMaPeriod = MaPeriod 14

mkMaConduit ::
  ( Monad m
  ) =>
  (a -> Candle) ->
  MaPeriod ->
  C.ConduitT a (a, Ma) m ()
mkMaConduit mkCandle per =
  C.slidingWindowC period
    .| ( whileM $ do
          mcandles <- fmap (>>= nonEmpty) C.await
          case mcandles of
            Just candles | length candles == period -> do
              C.yield
                ( last candles,
                  Ma
                    . QuotePerBase
                    $ ( sum
                          $ fmap
                            ( unQuotePerBase
                                . candleClose
                                . mkCandle
                            )
                            candles
                      )
                    / from @Natural @(Ratio Natural)
                      ( unsafeFrom
                          @Int
                          @Natural
                          period
                      )
                )
              pure True
            _ ->
              pure False
       )
  where
    period =
      case unsafeFrom @Natural @Int $ unMaPeriod per of
        x | x < 1 -> error $ "Bad MA period " <> inspect period
        x -> x

mkMa :: MaPeriod -> NonEmpty Candle -> Map UTCTime Ma
mkMa period candles =
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
    maPeriod = unsafeFrom @Natural @Int $ unMaPeriod period
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
      unsafeMa maPeriod candles stopAtIdx (currentIdx + 1)
        $ Map.insert maUtc maVal acc
  where
    chunk = V.slice currentIdx maPeriod candles
    maUtc = candleAt $ V.last chunk
    maVal =
      Ma
        . QuotePerBase
        . (/ Prelude.fromIntegral maPeriod)
        . V.foldl1 (+)
        $ V.map (unQuotePerBase . candleClose) chunk
