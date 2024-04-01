{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module Bfx.Parser
  ( parseOrder,
    parseOrderMap,
    parseCandle,
  )
where

import Bfx.Data.Metro
import Bfx.Data.Type
import Bfx.Import.External
import Data.Aeson.Lens
import qualified Data.Map as Map
import Lens.Micro hiding (at)
import qualified Prelude

parseOrder ::
  ( AsValue a,
    Show a,
    Data a
  ) =>
  a ->
  Either Text (SomeOrder 'Remote)
parseOrder x = do
  id0 <-
    maybeToRight (failure "OrderId is missing")
      $ OrderId
      <$> x
      ^? nth 0 . _Integral
  gid <-
    maybeToRight (failure "OrderGroupId is missing")
      $ (Just . OrderGroupId <$> x ^? nth 1 . _Integral)
      <|> ( ( either
                (const Nothing)
                (Just . Just . OrderGroupId)
                . readEither
            )
              =<< x
              ^? nth 1 . _String
          )
      <|> (Nothing <$ x ^? nth 1 . _Null)
  cid <-
    maybeToRight (failure "OrderClientId is missing")
      $ (Just . OrderClientId <$> x ^? nth 2 . _Integral)
      <|> (Nothing <$ x ^? nth 2 . _Null)
  sym0 <-
    maybeToRight (failure "Symbol is missing")
      $ x
      ^? nth 3 . _String
  sym <-
    first (const $ failure "Symbol is invalid")
      $ newCurrencyPair sym0
  amt0 <-
    maybeToRight (failure "OrderAmount is missing")
      $ toRational
      <$> x
      ^? nth 7 . _Number
  --
  -- TODO : handle zero amt???
  --
  -- SomeMoney bos amt <-
  --   first (failure . ("OrderAmount is invalid " <>) . inspect)
  --     $ tryFrom amt0
  ss0 <-
    maybeToRight (failure "OrderStatus is missing")
      $ x
      ^? nth 13 . _String
  ss1 <-
    first (failure . ("OrderStatus is not recognized " <>) . inspect)
      $ newOrderStatus ss0
  price <-
    maybeToRight
      (failure "ExchangeRate is missing")
      $ x
      ^? nth 16 . _Number
  rate <-
    first (const . failure $ "ExchangeRate is invalid " <> inspect price)
      $ tryFrom @Rational @(Ratio Natural) (toRational price)
  case newUnsignedMoneyBOS @(Tags 'Base) amt0 of
    SomeMoney bos amt ->
      pure
        . SomeOrder bos
        $ Order
          { orderId = id0,
            orderGroupId = gid,
            orderClientId = cid,
            orderAmount = amt,
            orderSymbol = sym,
            orderRate = newMoney rate,
            orderStatus = ss1
          }
  where
    -- let SomeMoney bos amt =
    --       newUnsignedMoneyBOS @(Tags 'Base) amt0 ::
    --         SomeMoney BuyOrSell (Tags 'Unsigned |+| 'Base)
    -- case bos of
    --   SBuy ->
    --     pure
    --       . SomeOrder SBuy
    --       $ Order
    --         { orderId = id0,
    --           orderGroupId = gid,
    --           orderClientId = cid,
    --           orderAmount = amt,
    --           orderSymbol = sym,
    --           orderRate = newMoney rate,
    --           orderStatus = ss1
    --         }

    failure =
      (<> " in " <> inspect x)

parseOrderMap ::
  ( AsValue a
  ) =>
  a ->
  Either Text (Map OrderId (SomeOrder 'Remote))
parseOrderMap raw = do
  xs <-
    maybeToRight
      "Json is not an array"
      $ raw
      ^? _Array
  foldrM parser mempty xs
  where
    parser x acc = do
      someOrder@(SomeOrder _ order) <- parseOrder x
      pure $ Map.insert (orderId order) someOrder acc

parseCandle ::
  ( AsValue a
  ) =>
  a ->
  Either Text Candle
parseCandle x = do
  at <-
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
      . newMoney
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
      . newMoney
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
      . newMoney
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
      . newMoney
      --
      -- TODO : tryFrom???
      --
      . unsafeFrom @Rational @(Ratio Natural)
      =<< maybeToRight
        "Low is missing"
        (toRational <$> x ^? nth 4 . _Number)
  vol <-
    first inspect
      . roundMoney
      . newMoney
      --
      -- TODO : tryFrom???
      --
      . unsafeFrom @Rational @(Ratio Natural)
      =<< maybeToRight
        "Volume is missing"
        (toRational <$> x ^? nth 5 . _Number)
  pure
    Candle
      { candleAt = at,
        candleOpen = open,
        candleClose = close,
        candleHigh = high,
        candleLow = low,
        candleVolume = vol
      }
