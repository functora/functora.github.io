{-# OPTIONS_HADDOCK show-extensions #-}

module Bfx.TestEnv
  ( adaBtc,
    btcAda,
    btcUsd,
    testAdaAmt,
  )
where

import Bfx
import Functora.Money
import Functora.Prelude

adaBtc :: CurrencyPair
adaBtc =
  either impureThrow id $ newCurrencyPair "ADABTC"

btcAda :: CurrencyPair
btcAda =
  either impureThrow id $ newCurrencyPair "BTCADA"

btcUsd :: CurrencyPair
btcUsd =
  either impureThrow id $ newCurrencyPair "BTCUSD"

testAdaAmt :: MoneyAmount
testAdaAmt =
  MoneyAmount 4.004004
