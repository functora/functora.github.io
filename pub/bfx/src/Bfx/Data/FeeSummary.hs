{-# OPTIONS_HADDOCK show-extensions #-}

module Bfx.Data.FeeSummary
  ( Response (..),
    getCryptoFee,
  )
where

import Bfx.Data.Type
import Functora.Money
import Functora.Prelude

--
-- TODO : this is not 100% correct
--
data Response = Response
  { makerCrypto2CryptoFee :: FeeRate,
    makerCrypto2StableFee :: FeeRate,
    makerCrypto2FiatFee :: FeeRate,
    makerDerivativeRebate :: RebateRate,
    takerCrypto2CryptoFee :: FeeRate,
    takerCrypto2StableFee :: FeeRate,
    takerCrypto2FiatFee :: FeeRate,
    takerDerivativeFee :: FeeRate
  }
  deriving stock
    ( Eq,
      Ord,
      Show,
      Generic
    )

getCryptoFee ::
  MakerOrTaker ->
  CurrencyKind ->
  Response ->
  FeeRate
getCryptoFee mot cck =
  case (mot, cck) of
    (Maker, Crypto) -> makerCrypto2CryptoFee
    (Maker, Stable) -> makerCrypto2StableFee
    (Maker, Fiat) -> makerCrypto2FiatFee
    (Taker, Crypto) -> takerCrypto2CryptoFee
    (Taker, Stable) -> takerCrypto2StableFee
    (Taker, Fiat) -> takerCrypto2FiatFee
