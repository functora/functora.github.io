{-# OPTIONS_HADDOCK show-extensions #-}

module Bfx.Indicator.Rsi
  ( Rsi (..),
    RsiPeriod (..),
    defRsiPeriod,
    mkRsiConduit,
  )
where

import Bfx.Data.Type
import Conduit ((.|))
import qualified Conduit as C
import qualified Data.Conduit.List as C
import Functora.Prelude
import qualified Prelude

newtype Rsi = Rsi
  { unRsi :: FixNonNeg
  }
  deriving stock
    ( Eq,
      Ord,
      Show,
      Read,
      Data,
      Generic
    )

newtype RsiPeriod = RsiPeriod
  { unRsiPeriod :: Natural
  }
  deriving stock
    ( Eq,
      Ord,
      Show,
      Read,
      Data,
      Generic
    )

defRsiPeriod :: RsiPeriod
defRsiPeriod = RsiPeriod 14

mkRsiConduit ::
  ( Monad m
  ) =>
  (a -> Candle) ->
  RsiPeriod ->
  C.ConduitT a (a, Rsi) m ()
mkRsiConduit mkCandle (RsiPeriod natPer) =
  C.slidingWindowC 2
    .| ( whileM $ do
          mcandles <- fmap (>>= nonEmpty) C.await
          case mcandles of
            Just [c1, c2] -> do
              let p1 = mkCandle c1 ^. #candleClose . #unQuotePerBase
              let p2 = mkCandle c2 ^. #candleClose . #unQuotePerBase
              C.yield
                ( c2,
                  -- Loss
                  if p1 >= p2
                    then p1 - p2
                    else 0,
                  -- Gain
                  if p1 <= p2
                    then p2 - p1
                    else 0
                )
              pure True
            _ ->
              pure False
       )
    .| ( do
          seed <- C.take intPer
          when (length seed == intPer) $ do
            let initAvgLoss = sum (fmap snd3 seed) / fixPer
            let initAvgGain = sum (fmap thd3 seed) / fixPer
            flip loopM (initAvgLoss, initAvgGain)
              $ \(prevAvgLoss, prevAvgGain) -> do
                mcandle <- C.await
                case mcandle of
                  Nothing -> pure $ Right ()
                  Just (c, loss, gain) -> do
                    let nextAvgLoss = (prevAvgLoss * (fixPer - 1) + loss) / fixPer
                    let nextAvgGain = (prevAvgGain * (fixPer - 1) + gain) / fixPer
                    let rs = nextAvgGain / nextAvgLoss
                    let rsi = Rsi $ 100 - (100 / (1 + rs))
                    C.yield (c, rsi)
                    pure $ Left (nextAvgLoss, nextAvgGain)
       )
  where
    fixPer :: FixNonNeg
    fixPer =
      Prelude.fromInteger $ from @Natural @Integer natPer
    intPer :: Int
    intPer =
      case unsafeFrom @Natural @Int natPer of
        x | x < 2 -> error $ "Bad RSI period " <> inspect natPer
        x -> x
