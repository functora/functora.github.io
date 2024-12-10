{-# OPTIONS_HADDOCK show-extensions #-}

module Bfx.Parser
  ( parseOrder,
    parseOrderMap,
    parseCandle,
  )
where

import Bfx.Data.Type
import Bfx.Math
import Data.Aeson.Lens
import qualified Data.Map as Map
import Functora.Money
import Functora.Prelude
import qualified Prelude

parseOrder ::
  (Show a, Data a, AsValue a) => a -> Either Text Order
parseOrder x = do
  id0 <-
    maybeToRight (failure "OrderId is missing")
      $ OrderId
      <$> x
      ^? nth 0
      . _Integral
  gid <-
    maybeToRight (failure "OrderGroupId is missing")
      $ (Just . OrderGroupId <$> x ^? nth 1 . _Integral)
      <|> ( ( either
                (const Nothing)
                (Just . Just . OrderGroupId)
                . readEither
            )
              =<< x
              ^? nth 1
              . _String
          )
      <|> (Nothing <$ x ^? nth 1 . _Null)
  cid <-
    maybeToRight (failure "OrderClientId is missing")
      $ (Just . OrderClientId <$> x ^? nth 2 . _Integral)
      <|> (Nothing <$ x ^? nth 2 . _Null)
  sym0 <-
    maybeToRight (failure "Symbol is missing")
      $ x
      ^? nth 3
      . _String
  sym <-
    first (const $ failure "Symbol is invalid")
      $ newCurrencyPair sym0
  amt0 <-
    maybeToRight (failure "OrderAmount is missing")
      $ toRational
      <$> x
      ^? nth 7
      . _Number
  let amt =
        MoneyAmount
          . unsafeFrom @Rational @(Ratio Natural)
          $ abs amt0
  ss0 <-
    maybeToRight (failure "OrderStatus is missing")
      $ x
      ^? nth 13
      . _String
  ss1 <-
    first (failure . ("OrderStatus is not recognized " <>) . inspect)
      $ newOrderStatus ss0
  price <-
    maybeToRight
      (failure "ExchangeRate is missing")
      $ x
      ^? nth 16
      . _Number
  rate <-
    bimap
      (const . failure $ "ExchangeRate is invalid " <> inspect price)
      QuotePerBase
      $ tryFrom @Rational @(Ratio Natural) (toRational price)
  let mkOrder bos =
        Order
          { orderId = id0,
            orderGroupId = gid,
            orderClientId = cid,
            orderBaseAmount = amt,
            orderSymbol = sym,
            orderRate = rate,
            orderStatus = ss1,
            orderBuyOrSell = bos,
            orderLocalOrRemote = Remote
          }
  if
    | amt0 > 0 -> pure $ mkOrder Buy
    | amt0 < 0 -> pure $ mkOrder Sell
    | otherwise -> Left "Got zero money amount"
  where
    failure = (<> " in " <> inspect x)

parseOrderMap ::
  ( AsValue a
  ) =>
  a ->
  Either Text (Map OrderId Order)
parseOrderMap raw = do
  xs <-
    maybeToRight
      "Json is not an array"
      $ raw
      ^? _Array
  foldrM parser mempty xs
  where
    parser x acc = do
      order <- parseOrder x
      pure $ Map.insert (orderId order) order acc

parseCandle ::
  ( AsValue a
  ) =>
  a ->
  Either Text Candle
parseCandle x = do
  utc <-
    posixSecondsToUTCTime
      . (/ 1000)
      . Prelude.fromInteger
      . from @Natural
      <$> maybeToRight
        "UTCTime is missing"
        (x ^? nth 0 . _Integral)
  open <-
    first inspect
      . roundQuotePerBase
      . QuotePerBase
      --
      -- TODO : tryFrom???
      --
      . unsafeFrom @Rational @(Ratio Natural)
      =<< maybeToRight
        "Open is missing"
        (toRational <$> x ^? nth 1 . _Number)
  close <-
    first inspect
      . roundQuotePerBase
      . QuotePerBase
      --
      -- TODO : tryFrom???
      --
      . unsafeFrom @Rational @(Ratio Natural)
      =<< maybeToRight
        "Close is missing"
        (toRational <$> x ^? nth 2 . _Number)
  high <-
    first inspect
      . roundQuotePerBase
      . QuotePerBase
      --
      -- TODO : tryFrom???
      --
      . unsafeFrom @Rational @(Ratio Natural)
      =<< maybeToRight
        "High is missing"
        (toRational <$> x ^? nth 3 . _Number)
  low <-
    first inspect
      . roundQuotePerBase
      . QuotePerBase
      --
      -- TODO : tryFrom???
      --
      . unsafeFrom @Rational @(Ratio Natural)
      =<< maybeToRight
        "Low is missing"
        (toRational <$> x ^? nth 4 . _Number)
  vol <-
    first inspect
      . roundMoneyAmount
      . MoneyAmount
      --
      -- TODO : tryFrom???
      --
      . unsafeFrom @Rational @(Ratio Natural)
      =<< maybeToRight
        "Volume is missing"
        (toRational <$> x ^? nth 5 . _Number)
  pure
    Candle
      { candleAt = utc,
        candleOpen = open,
        candleClose = close,
        candleHigh = high,
        candleLow = low,
        candleBaseVolume = vol
      }
