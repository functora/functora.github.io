module Functora.Miso.Jsm
  ( popupText,
    shareText,
    addFieldPair,
    addAsset,
    addPaymentMethod,
    moveUp,
    moveDown,
    removeAt,
    duplicateAt,
    openBrowserPage,
    enterOrEscapeBlur,
  )
where

import qualified Data.Generics as Syb
import Functora.Miso.Prelude
import Functora.Miso.Types
import Functora.Money (CurrencyCode (..), CurrencyInfo (..))
import qualified Language.Javascript.JSaddle as JS
import qualified Text.URI as URI
import qualified Prelude ((!!))

popupText :: (Show a, Data a) => a -> JSM ()
popupText x =
  void
    $ JS.global
    ^. JS.js1 ("popupText" :: MisoString) (inspect x :: MisoString)

shareText :: (Show a, Data a) => a -> JSM (model -> model)
shareText x = do
  let txt = inspect x
  unless (txt == mempty) $ do
    prom <- JS.global ^. JS.js1 ("shareText" :: MisoString) txt
    success <- JS.function $ \_ _ _ -> popupText @MisoString "Copied!"
    failure <- JS.function $ \_ _ _ -> popupText @MisoString "Failed to copy!"
    void $ prom ^. JS.js2 ("then" :: MisoString) success failure
  pure id

addFieldPair ::
  ATraversal' model [FieldPair DynamicField Unique] -> JSM (model -> model)
addFieldPair optic = do
  popupText @MisoString "Added note!"
  item <- newFieldPair mempty $ DynamicFieldText mempty
  pure (& cloneTraversal optic %~ (<> [item]))

addAsset :: ATraversal' model [Asset Unique] -> JSM (model -> model)
addAsset optic = do
  popupText @MisoString "Added asset!"
  let cur = CurrencyInfo (CurrencyCode "usd") mempty
  item <- newAsset "Price" 0 cur
  pure (& cloneTraversal optic %~ (<> [item]))

addPaymentMethod ::
  ATraversal' model [PaymentMethod Unique] ->
  JSM (model -> model)
addPaymentMethod optic = do
  popupText @MisoString "Added payment!"
  let cur = CurrencyInfo (CurrencyCode "btc") mempty
  item <- newPaymentMethod cur $ Just mempty
  pure (& cloneTraversal optic %~ (<> [item]))

moveUp :: ATraversal' model [item] -> Int -> JSM (model -> model)
moveUp optic idx = do
  popupText @MisoString $ "Moved #" <> inspect (idx + 1) <> " up!"
  pure (& cloneTraversal optic %~ swapAt (idx - 1) idx)

moveDown :: ATraversal' model [item] -> Int -> JSM (model -> model)
moveDown optic idx = do
  popupText @MisoString $ "Moved #" <> inspect (idx + 1) <> " down!"
  pure (& cloneTraversal optic %~ swapAt idx (idx + 1))

removeAt :: ATraversal' model [a] -> Int -> JSM (model -> model)
removeAt optic idx = do
  popupText @MisoString $ "Removed #" <> inspect (idx + 1) <> "!"
  pure (& cloneTraversal optic %~ ((>>= uncurry updater) . zip [0 ..]))
  where
    updater loc el =
      if loc == idx
        then mempty
        else [el]

duplicateAt ::
  forall model item.
  ( Data item
  ) =>
  ATraversal' model [item] ->
  Int ->
  JSM (model -> model)
duplicateAt optic idx = do
  popupText @MisoString $ "Duplicated #" <> inspect (idx + 1) <> "!"
  duplicator <- newUniqueDuplicator @MisoString
  let updater loc el =
        if loc == idx
          then [el, closed $ duplicator el]
          else [el]
  pure (& cloneTraversal optic %~ ((>>= uncurry updater) . zip [0 ..]))
  where
    closed :: item -> item
    closed = Syb.everywhere $ Syb.mkT $ const Closed

swapAt :: Int -> Int -> [a] -> [a]
swapAt i j xs
  | i == j = xs
  | i < 0 || i >= len = xs
  | j < 0 || j >= len = xs
  | otherwise = do
      (idx, val) <- zip [0 ..] xs
      pure
        $ if
          | idx == i -> jval
          | idx == j -> ival
          | otherwise -> val
  where
    len = length xs
    ival = xs Prelude.!! i
    jval = xs Prelude.!! j

openBrowserPage :: URI -> JSM (model -> model)
openBrowserPage uri = do
  void $ JS.global ^. JS.js1 @MisoString "openBrowserPage" (URI.render uri)
  pure id

enterOrEscapeBlur :: Uid -> KeyCode -> JSM (model -> model)
enterOrEscapeBlur uid (KeyCode code) = do
  let enterOrEscape = [13, 27] :: [Int]
  when (code `elem` enterOrEscape)
    . void
    . JS.eval @MisoString
    $ "document.getElementById('"
    <> htmlUid uid
    <> "').getElementsByTagName('input')[0].blur();"
  pure id
