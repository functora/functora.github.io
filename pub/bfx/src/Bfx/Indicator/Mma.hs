{-# OPTIONS_HADDOCK show-extensions #-}

module Bfx.Indicator.Mma
  ( RewardToRisk (..),
    TradeEntry (..),
    TradeExit (..),
    StopLoss (..),
    TakeProfit (..),
    Mma (..),
    mma,
  )
where

import Bfx.Import
import Bfx.Indicator.Atr (Atr)
import qualified Bfx.Indicator.Atr as Atr
import Bfx.Indicator.Ma
import qualified Data.Map as Map
import qualified Data.Ord as Ord
import qualified Data.Vector as V
import qualified Math.Combinat.Sets as Math

newtype RewardToRisk = RewardToRisk
  { unRewardToRisk :: Rational
  }
  deriving newtype
    ( Eq,
      Ord,
      NFData
    )
  deriving stock
    ( Generic,
      Show
    )

newtype CrvQty = CrvQty
  { unCrvQty :: Int
  }
  deriving newtype
    ( Eq,
      Ord,
      Num,
      Real,
      Enum,
      Integral
    )
  deriving stock
    ( Generic,
      Show
    )

data TradeEntry = TradeEntry
  { tradeEntryCandle :: Candle,
    tradeEntryAtr :: Atr,
    tradeEntryPrevSwingLow :: PrevSwingLow,
    tradeEntryStopLoss :: StopLoss,
    tradeEntryTakeProfit :: TakeProfit,
    tradeEntryProfitRate :: ProfitRate
  }
  deriving stock
    ( Eq,
      Ord,
      Generic
    )

instance NFData TradeEntry

newtype TradeExit = TradeExit
  { unTradeExit :: Candle
  }
  deriving newtype
    ( Eq,
      Ord,
      NFData
    )
  deriving stock
    ( Generic,
      Show
    )

newtype PrevSwingLow = PrevSwingLow
  { unPrevSwingLow :: Candle
  }
  deriving newtype
    ( Eq,
      Ord,
      NFData
    )
  deriving stock
    ( Generic,
      Show
    )

newtype TakeProfit = TakeProfit
  { unTakeProfit :: QuotePerBase'
  }
  deriving newtype
    ( Eq,
      Ord,
      NFData
    )
  deriving stock
    ( Generic
    )

newtype StopLoss = StopLoss
  { unStopLoss :: QuotePerBase'
  }
  deriving newtype
    ( Eq,
      Ord,
      NFData
    )
  deriving stock
    ( Generic
    )

data Mma = Mma
  { mmaSymbol :: CurrencyPair,
    mmaCandles :: NonEmpty Candle,
    mmaCurves :: Map MaPeriod (Map UTCTime Ma),
    mmaTrades :: [(TradeEntry, TradeExit)],
    mmaRewardToRisk :: RewardToRisk,
    mmaEntry :: TradeEntry,
    mmaDataFrom :: UTCTime,
    mmaCtf :: CandleTimeFrame,
    mmaAt :: UTCTime
  }
  deriving stock
    ( Eq,
      Generic
    )

instance NFData Mma

--
-- TODO : use r2r!!!
--
instance Ord Mma where
  compare lhs rhs =
    compare
      (length $ mmaTrades lhs, mmaRewardToRisk lhs)
      (length $ mmaTrades rhs, mmaRewardToRisk rhs)

mma ::
  ProfitRateB 'Min ->
  ProfitRateB 'Max ->
  CandleTimeFrame ->
  CurrencyPair ->
  NonEmpty Candle ->
  Maybe Mma
mma minProf maxProf ctf sym cs =
  (maximum <$>) . nonEmpty $
    [6 .. 9]
      >>= combineMaPeriods minProf maxProf ctf sym cs atr
  where
    atr =
      Atr.atr cs

combineMaPeriods ::
  ProfitRateB 'Min ->
  ProfitRateB 'Max ->
  CandleTimeFrame ->
  CurrencyPair ->
  NonEmpty Candle ->
  Map UTCTime Atr ->
  CrvQty ->
  [Mma]
combineMaPeriods minProf maxProf ctf sym cs atrs qty =
  mapMaybe
    ( newMma minProf maxProf ctf sym cs atrs
        . V.indexed
        . V.fromList
        $ toList cs
    )
    . catMaybes
    . (nonEmpty <$>)
    . Math.choose (unCrvQty qty)
    . ((\p -> (p, ma p cs)) <$>)
    $ [2 .. 17]

newMma ::
  ProfitRateB 'Min ->
  ProfitRateB 'Max ->
  CandleTimeFrame ->
  CurrencyPair ->
  NonEmpty Candle ->
  Map UTCTime Atr ->
  Vector (Int, Candle) ->
  NonEmpty (MaPeriod, Map UTCTime Ma) ->
  Maybe Mma
newMma minProf maxProf ctf sym cs0 atrs cs curves = do
  (csPrev, cLast) <- V.unsnoc cs
  (_, cPrev) <- V.unsnoc csPrev
  let newEntry r2r =
        tryFindEntries
          minProf
          maxProf
          r2r
          cs
          [cPrev, cLast]
          atrs
          curves
          V.!? 0
  dummyEntry <-
    newEntry $ RewardToRisk 1
  maxMma <-
    (maximum <$>)
      . nonEmpty
      . catMaybes
      $ ( \rate -> do
            let r2r = RewardToRisk $ rate % 5
            trades <-
              V.mapM (tryFindExit csPrev) $
                tryFindEntries
                  minProf
                  maxProf
                  r2r
                  cs
                  csPrev
                  atrs
                  curves
            mas <-
              nonEmpty $ Map.assocs shortestCurve
            pure
              Mma
                { mmaSymbol = sym,
                  mmaCandles = cs0,
                  mmaCurves = Map.fromList $ from curves,
                  mmaTrades = V.toList trades,
                  mmaRewardToRisk = r2r,
                  mmaEntry = snd dummyEntry,
                  mmaDataFrom = minimum $ fst <$> mas,
                  mmaCtf = ctf,
                  mmaAt =
                    candleAt
                      . tradeEntryCandle
                      $ snd dummyEntry
                }
        )
        <$> [15, 14 .. 8]
  if length (mmaTrades maxMma) < 6
    then Nothing
    else do
      entry <-
        newEntry $ mmaRewardToRisk maxMma
      pure
        maxMma
          { mmaEntry = snd entry,
            mmaAt =
              candleAt
                . tradeEntryCandle
                $ snd entry
          }
  where
    shortestCurve =
      snd $
        maximumBy
          ( \lhs rhs ->
              compare (fst lhs) (fst rhs)
          )
          curves

tryFindEntries ::
  ProfitRateB 'Min ->
  ProfitRateB 'Max ->
  RewardToRisk ->
  Vector (Int, Candle) ->
  Vector (Int, Candle) ->
  Map UTCTime Atr ->
  NonEmpty (MaPeriod, Map UTCTime Ma) ->
  Vector (Int, TradeEntry)
tryFindEntries minProf maxProf r2r csHist cs atrs curves =
  ( \((_, c0), (idx1, c1)) ->
      let at0 = candleAt c0
          mas0 = newMas at0
          at1 = candleAt c1
          mas1 = newMas at1
       in if (length curves == length mas0)
            && (length curves == length mas1)
            && goodCandle c0 c1 mas1
            && not (goodCandle c0 c1 mas0)
            then fromMaybe mempty $ do
              atr <- Map.lookup at1 atrs
              prv <- tryFindPrevSwingLow (V.take idx1 csHist) c1
              stopLoss <- tryFindStopLoss prv atr
              takeProfit <- tryFindTakeProfit r2r c1 stopLoss
              let profRate =
                    ProfitRate . unUnitless $
                      ( unTakeProfit takeProfit
                          |-| unQuotePerBase (candleClose c1)
                      )
                        |/| unQuotePerBase (candleClose c1)
              if profRate < unProfitRateB minProf
                || profRate > unProfitRateB maxProf
                then mempty
                else
                  pure . V.singleton $
                    ( idx1,
                      TradeEntry
                        { tradeEntryCandle = c1,
                          tradeEntryAtr = atr,
                          tradeEntryPrevSwingLow = prv,
                          tradeEntryStopLoss = stopLoss,
                          tradeEntryTakeProfit = takeProfit,
                          tradeEntryProfitRate = profRate
                        }
                    )
            else mempty
  )
    <=< V.zip cs
    $ V.tail cs
  where
    newMas :: UTCTime -> [(MaPeriod, Ma)]
    newMas t =
      mapMaybe
        (\(p, curve) -> (p,) <$> Map.lookup t curve)
        $ toList curves

goodCandle ::
  Candle ->
  Candle ->
  [(MaPeriod, Ma)] ->
  Bool
goodCandle x0 x1 xs =
  (c1 > c0)
    && all ((unQuotePerBase c1 >) . unMa . snd) xs
    && (xs == sortOn (Ord.Down . snd) xs)
  where
    c0 = candleClose x0
    c1 = candleClose x1

tryFindPrevSwingLow ::
  Vector (Int, Candle) ->
  Candle ->
  Maybe PrevSwingLow
tryFindPrevSwingLow cs cur = do
  (hist, (_, prev)) <- V.unsnoc cs
  if candleLow prev <= candleLow cur
    then tryFindPrevSwingLow hist prev
    else pure $ PrevSwingLow cur

tryFindStopLoss :: PrevSwingLow -> Atr -> Maybe StopLoss
tryFindStopLoss prevLow0 atr0 =
  if prevLow <= volatility
    then Nothing
    else
      Just . StopLoss $
        prevLow |-| volatility
  where
    volatility =
      1.8 *| Atr.unAtr atr0
    prevLow =
      unQuotePerBase . candleLow $
        unPrevSwingLow prevLow0

tryFindTakeProfit ::
  RewardToRisk ->
  Candle ->
  StopLoss ->
  Maybe TakeProfit
tryFindTakeProfit r2r entryCandle stopLoss =
  if current <= stop
    then Nothing
    else
      Just . TakeProfit $
        current |+| ((current |-| stop) |* unRewardToRisk r2r)
  where
    stop =
      unStopLoss stopLoss
    current =
      unQuotePerBase $ candleClose entryCandle

tryFindExit ::
  Vector (Int, Candle) ->
  (Int, TradeEntry) ->
  Maybe (TradeEntry, TradeExit)
tryFindExit cs (idx, entry) =
  tryFindExit' entry
    . toList
    . (snd <$>)
    $ V.drop idx cs

tryFindExit' ::
  TradeEntry ->
  [Candle] ->
  Maybe (TradeEntry, TradeExit)
tryFindExit' _ [] = Nothing
tryFindExit' entr (x : xs)
  | unQuotePerBase (candleLow x)
      < unStopLoss (tradeEntryStopLoss entr) =
      Nothing
  | unQuotePerBase (candleHigh x)
      > unTakeProfit (tradeEntryTakeProfit entr) =
      pure (entr, TradeExit x)
  | otherwise =
      tryFindExit' entr xs
