{-# OPTIONS_HADDOCK show-extensions #-}

module Bfx.TestEnv
  ( adaBtc,
    btcAda,
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

testAdaAmt :: MoneyAmount
testAdaAmt =
  MoneyAmount 4.004004
