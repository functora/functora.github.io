{-# OPTIONS_HADDOCK show-extensions #-}

module BitfinexClient.Data.FeeSummary
  ( Response (..),
    getFee,
  )
where

import BitfinexClient.Data.Kind
import BitfinexClient.Data.Type
import BitfinexClient.Import.External

data Response = Response
  { makerCrypto2CryptoFee :: FeeRate 'Maker 'Base,
    makerCrypto2StableFee :: FeeRate 'Maker 'Base,
    makerCrypto2FiatFee :: FeeRate 'Maker 'Base,
    makerDerivativeRebate :: RebateRate 'Maker,
    takerCrypto2CryptoFee :: FeeRate 'Taker 'Base,
    takerCrypto2StableFee :: FeeRate 'Taker 'Base,
    takerCrypto2FiatFee :: FeeRate 'Taker 'Base,
    takerDerivativeFee :: FeeRate 'Taker 'Base
  }
  deriving stock
    ( Eq,
      Ord,
      Show,
      Generic
    )

getFee ::
  forall (mrel :: MarketRelation).
  ( SingI mrel
  ) =>
  CurrencyKind ->
  Response ->
  FeeRate mrel 'Base
getFee cck =
  case (sing :: Sing mrel, cck) of
    (SMaker, Crypto) -> makerCrypto2CryptoFee
    (SMaker, Stable) -> makerCrypto2StableFee
    (SMaker, Fiat) -> makerCrypto2FiatFee
    (STaker, Crypto) -> takerCrypto2CryptoFee
    (STaker, Stable) -> takerCrypto2StableFee
    (STaker, Fiat) -> takerCrypto2FiatFee
