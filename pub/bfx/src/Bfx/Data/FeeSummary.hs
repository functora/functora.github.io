{-# OPTIONS_HADDOCK show-extensions #-}

module Bfx.Data.FeeSummary
  ( Response (..),
    getFee,
  )
where

import Bfx.Data.Kind
import Bfx.Data.Type
import Bfx.Import.External

--
-- TODO : this is not 100% correct
--
data Response = Response
  { makerCrypto2CryptoFee :: Money (Tags 'Unsigned |+| 'FeeRate |+| 'Maker),
    makerCrypto2StableFee :: Money (Tags 'Unsigned |+| 'FeeRate |+| 'Maker),
    makerCrypto2FiatFee :: Money (Tags 'Unsigned |+| 'FeeRate |+| 'Maker),
    makerDerivativeRebate :: RebateRate 'Maker,
    takerCrypto2CryptoFee :: Money (Tags 'Unsigned |+| 'FeeRate |+| 'Taker),
    takerCrypto2StableFee :: Money (Tags 'Unsigned |+| 'FeeRate |+| 'Taker),
    takerCrypto2FiatFee :: Money (Tags 'Unsigned |+| 'FeeRate |+| 'Taker),
    takerDerivativeFee :: Money (Tags 'Unsigned |+| 'FeeRate |+| 'Taker)
  }
  deriving stock
    ( Eq,
      Ord,
      Show,
      Generic
    )

--
-- TODO : accept 2 types
--
getFee ::
  forall (mot :: MakerOrTaker).
  ( SingI mot
  ) =>
  CurrencyKind ->
  Response ->
  Money (Tags 'Unsigned |+| 'FeeRate |+| mot)
getFee ck =
  case (sing :: Sing mot, ck) of
    (SMaker, Crypto) -> makerCrypto2CryptoFee
    (SMaker, Stable) -> makerCrypto2StableFee
    (SMaker, Fiat) -> makerCrypto2FiatFee
    (STaker, Crypto) -> takerCrypto2CryptoFee
    (STaker, Stable) -> takerCrypto2StableFee
    (STaker, Fiat) -> takerCrypto2FiatFee
