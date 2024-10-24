module Functora.Miso.Jsm.Specific
  ( addFieldPair,
    duplicateAt,
  )
where

import qualified Data.Generics as Syb
import qualified Functora.Miso.Jsm.Generic as Jsm
import Functora.Miso.Prelude
import Functora.Miso.Types

addFieldPair ::
  ATraversal' model [FieldPair DynamicField Unique] -> Update model
addFieldPair optic =
  ImpureUpdate $ do
    item <- newFieldPair mempty $ DynamicFieldText mempty
    Jsm.popupText @Unicode "Added note!"
    pure $ cloneTraversal optic %~ (<> [item])

duplicateAt ::
  forall model item.
  ( Data item
  ) =>
  ATraversal' model [item] ->
  Int ->
  Update model
duplicateAt optic idx =
  ImpureUpdate $ do
    duplicator <- newUniqueDuplicator @Unicode
    let updater loc el =
          if loc == idx
            then [el, closed $ duplicator el]
            else [el]
    Jsm.popupText @Unicode $ "Duplicated #" <> inspect (idx + 1) <> "!"
    pure $ cloneTraversal optic %~ ((>>= uncurry updater) . zip [0 ..])
  where
    closed :: item -> item
    closed = Syb.everywhere $ Syb.mkT $ const Closed
