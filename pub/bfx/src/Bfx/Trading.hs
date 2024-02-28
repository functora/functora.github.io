{-# OPTIONS_HADDOCK show-extensions #-}

module Bfx.Trading
  ( theBestMma,
  )
where

import qualified Bfx as Bfx
import qualified Bfx.Data.Candles as Candles
import Bfx.Import
import Bfx.Indicator.Mma (Mma)
import qualified Bfx.Indicator.Mma as Mma
import qualified Control.Parallel.Strategies as Par
import qualified Data.List.NonEmpty.Extra as NE
import qualified Data.Map as Map
import qualified Data.Set as Set

theBestMma ::
  ( MonadIO m
  ) =>
  ProfitRateB 'Min ->
  ProfitRateB 'Max ->
  CandleTimeFrame ->
  Money 'Quote 'Buy ->
  Set (CurrencyCode 'Base) ->
  CurrencyCode 'Quote ->
  ExceptT Error m Mma
theBestMma minProf maxProf ctf vol blacklist quote = do
  tickers <-
    Bfx.tickers
  let goodTickers =
        Map.filter
          ( \x ->
              ( ( unQuotePerBase (tickerBid x)
                    |*| unMoney @'Base (tickerVolume x)
                )
                  > unMoney vol
              )
                && ( currencyPairQuote
                      (tickerSymbol x)
                      == quote
                   )
                && not
                  ( currencyPairBase
                      (tickerSymbol x)
                      `Set.member` blacklist
                  )
          )
          tickers
  syms <-
    Bfx.symbolsDetails
  cs <-
    mapM
      ( \sym -> do
          liftIO $ threadDelay 500000
          (sym,)
            <$> Bfx.candlesHist
              ctf
              sym
              Candles.optsDef
                { Candles.limit = Just 1000
                }
      )
      . filter (`Map.member` goodTickers)
      $ Map.keys syms
  case nonEmpty cs of
    Nothing ->
      throwE $
        ErrorTrading quote "Can not find trading pairs"
    Just ncs -> do
      mma <-
        ExceptT
          . pure
          . maybeToRight (ErrorTrading quote "No any good Mma")
          . (maximum <$>)
          . nonEmpty
          . catMaybes
          . Par.withStrategy (Par.parTraversable Par.rdeepseq)
          $ uncurry (Mma.mma minProf maxProf ctf) <$> toList ncs
      let sym =
            Mma.mmaSymbol mma
      cfg <-
        ExceptT
          . pure
          . maybeToRight
            ( ErrorTrading quote $
                "Can not find symbol datails for " <> show sym
            )
          $ Map.lookup sym syms
      --
      -- TODO : verify by backtesting
      --
      avg <-
        Bfx.marketAveragePrice (currencyPairMinOrderAmt cfg) sym
      let candles =
            Mma.mmaCandles mma
      let history =
            init candles
      let entry =
            last candles
      ExceptT
        . pure
        . maybeToRight
          ( ErrorTrading quote $
              "Can not verify Mma for " <> show sym
          )
        . Mma.mma minProf maxProf ctf sym
        $ NE.appendr history [entry {candleClose = avg}]
