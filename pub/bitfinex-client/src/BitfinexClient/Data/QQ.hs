{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module BitfinexClient.Data.QQ
  ( mkTryQQ,
    mkTryRatQQ,
    --feeRate,
    feeRateMakerBase,
    feeRateMakerQuote,
    feeRateTakerBase,
    feeRateTakerQuote,
    profitRate,
    ccBase,
    ccQuote,
    currencyPair,
  )
where

import BitfinexClient.Data.Kind
import BitfinexClient.Data.Type
import BitfinexClient.Import.External
import BitfinexClient.Util
import qualified Data.Text as T
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax as TH (Lift, lift)

showError ::
  forall source target.
  ( Show source,
    Typeable target
  ) =>
  Either (TryFromException source target) target ->
  Either String target
showError =
  first $ \(TryFromException x _) ->
    showType @target
      <> " can not be read from "
      <> show x

mkQQ ::
  forall target.
  ( TH.Lift target,
    Typeable target
  ) =>
  (String -> Either String target) ->
  QuasiQuoter
mkQQ parser =
  QuasiQuoter
    { quoteDec = failure "quoteDec",
      quoteType = failure "quoteType",
      quotePat = failure "quotePat",
      quoteExp =
        \x0 ->
          case parser x0 of
            Left e -> fail e
            Right x -> [|$(TH.lift x)|]
    }
  where
    failure :: Text -> a
    failure field =
      error $
        showType @target
          <> " "
          <> field
          <> " is not implemented"

mkTryQQ ::
  forall through target.
  ( Read through,
    TryFrom through target,
    TH.Lift target,
    Typeable target,
    'False ~ (String == through),
    'False ~ (through == target)
  ) =>
  QuasiQuoter
mkTryQQ =
  mkQQ $
    showError . tryReadVia @through @target

mkTryRatQQ ::
  forall through target.
  ( Read through,
    Fractional through,
    TryFrom through target,
    TH.Lift target,
    Typeable target,
    'False ~ (String == through),
    'False ~ (through == target)
  ) =>
  QuasiQuoter
mkTryRatQQ =
  mkQQ $ \x ->
    showError (tryReadViaRatio @through @target x)
      <|> showError (tryReadVia @through @target x)

--
-- TODO : for some reason this is not working:
-- No instance for (Typeable mr0) arising from a use of ‘feeRate’
--
-- feeRate ::
--   forall (mr :: MarketRelation) (cr :: CurrencyRelation).
--   ( Typeable mr,
--     Typeable cr
--   ) =>
--   QuasiQuoter
-- feeRate =
--   mkTryRatQQ @Rational @(FeeRate mr cr)

feeRateMakerBase :: QuasiQuoter
feeRateMakerBase =
  mkTryRatQQ @Rational @(FeeRate 'Maker 'Base)

feeRateMakerQuote :: QuasiQuoter
feeRateMakerQuote =
  mkTryRatQQ @Rational @(FeeRate 'Maker 'Quote)

feeRateTakerBase :: QuasiQuoter
feeRateTakerBase =
  mkTryRatQQ @Rational @(FeeRate 'Taker 'Base)

feeRateTakerQuote :: QuasiQuoter
feeRateTakerQuote =
  mkTryRatQQ @Rational @(FeeRate 'Taker 'Quote)

profitRate :: QuasiQuoter
profitRate =
  mkTryRatQQ @Rational @ProfitRate

--
-- TODO : use phantom type in QQ
--
ccBase :: QuasiQuoter
ccBase =
  mkQQ $
    showError
      . newCurrencyCode @'Base
      . T.pack

ccQuote :: QuasiQuoter
ccQuote =
  mkQQ $
    showError
      . newCurrencyCode @'Quote
      . T.pack

currencyPair :: QuasiQuoter
currencyPair =
  mkQQ $
    showError
      . newCurrencyPair
      . T.pack
