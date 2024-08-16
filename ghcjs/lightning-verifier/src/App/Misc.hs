module App.Misc
  ( getConverterAmountOptic,
    getConverterCurrencyOptic,
    pushActionQueue,
  )
where

import App.Types
import Functora.Miso.Prelude

getConverterAmountOptic ::
  ( Functor f
  ) =>
  TopOrBottom ->
  LensLike' f Model (Field Rational Unique)
getConverterAmountOptic = \case
  Top -> #modelState . #stDoc . #stDocTopMoney . #moneyAmount
  Bottom -> #modelState . #stDoc . #stDocBottomMoney . #moneyAmount

getConverterCurrencyOptic ::
  ( Functor f
  ) =>
  TopOrBottom ->
  LensLike' f Model (Currency Unique)
getConverterCurrencyOptic = \case
  Top -> #modelState . #stDoc . #stDocTopMoney . #moneyCurrency
  Bottom -> #modelState . #stDoc . #stDocBottomMoney . #moneyCurrency

pushActionQueue ::
  ( MonadIO m
  ) =>
  Model ->
  InstantOrDelayed (Model -> JSM Model) ->
  m ()
pushActionQueue st =
  liftIO
    . atomically
    . writeTChan (st ^. #modelProducerQueue)
