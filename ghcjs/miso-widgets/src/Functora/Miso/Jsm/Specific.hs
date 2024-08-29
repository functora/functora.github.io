module Functora.Miso.Jsm.Specific
  ( addFieldPair,
    addAsset,
    addPaymentMethod,
    duplicateAt,
  )
where

import qualified Data.Generics as Syb
import qualified Functora.Miso.Jsm.Generic as Jsm
import Functora.Miso.Prelude
import Functora.Miso.Types
import Functora.Money (CurrencyCode (..), CurrencyInfo (..))

addFieldPair ::
  ATraversal' model [FieldPair DynamicField Unique] -> model -> JSM model
addFieldPair optic st = do
  item <- newFieldPair mempty $ DynamicFieldText mempty
  Jsm.popupText @MisoString "Added note!"
  pure $ st & cloneTraversal optic %~ (<> [item])

addAsset :: ATraversal' model [Asset Unique] -> model -> JSM model
addAsset optic st = do
  let cur = CurrencyInfo (CurrencyCode "usd") mempty
  item <- newAsset "Price" 0 cur
  Jsm.popupText @MisoString "Added asset!"
  pure $ st & cloneTraversal optic %~ (<> [item])

addPaymentMethod ::
  ATraversal' model [PaymentMethod Unique] -> model -> JSM model
addPaymentMethod optic st = do
  let cur = CurrencyInfo (CurrencyCode "btc") mempty
  item <- newPaymentMethod cur $ Just mempty
  Jsm.popupText @MisoString "Added payment!"
  pure $ st & cloneTraversal optic %~ (<> [item])

duplicateAt ::
  forall model item.
  ( Data item
  ) =>
  ATraversal' model [item] ->
  Int ->
  model ->
  JSM model
duplicateAt optic idx st = do
  duplicator <- newUniqueDuplicator @MisoString
  let updater loc el =
        if loc == idx
          then [el, closed $ duplicator el]
          else [el]
  Jsm.popupText @MisoString $ "Duplicated #" <> inspect (idx + 1) <> "!"
  pure $ st & cloneTraversal optic %~ ((>>= uncurry updater) . zip [0 ..])
  where
    closed :: item -> item
    closed = Syb.everywhere $ Syb.mkT $ const Closed
