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

shareText :: (Show a, Data a) => a -> model -> JSM model
shareText x st = do
  let txt = inspect x
  unless (txt == mempty) $ do
    prom <- JS.global ^. JS.js1 ("shareText" :: MisoString) txt
    success <- JS.function $ \_ _ _ -> popupText @MisoString "Copied!"
    failure <- JS.function $ \_ _ _ -> popupText @MisoString "Failed to copy!"
    void $ prom ^. JS.js2 ("then" :: MisoString) success failure
  pure st

addFieldPair ::
  ATraversal' model [FieldPair DynamicField Unique] -> model -> JSM model
addFieldPair optic st = do
  item <- newFieldPair mempty $ DynamicFieldText mempty
  popupText @MisoString "Added note!"
  pure $ st & cloneTraversal optic %~ (<> [item])

addAsset :: ATraversal' model [Asset Unique] -> model -> JSM model
addAsset optic st = do
  let cur = CurrencyInfo (CurrencyCode "usd") mempty
  item <- newAsset "Price" 0 cur
  popupText @MisoString "Added asset!"
  pure $ st & cloneTraversal optic %~ (<> [item])

addPaymentMethod ::
  ATraversal' model [PaymentMethod Unique] -> model -> JSM model
addPaymentMethod optic st = do
  let cur = CurrencyInfo (CurrencyCode "btc") mempty
  item <- newPaymentMethod cur $ Just mempty
  popupText @MisoString "Added payment!"
  pure $ st & cloneTraversal optic %~ (<> [item])

moveUp :: ATraversal' model [item] -> Int -> model -> JSM model
moveUp optic idx st = do
  popupText @MisoString $ "Moved #" <> inspect (idx + 1) <> " up!"
  pure $ st & cloneTraversal optic %~ swapAt (idx - 1) idx

moveDown :: ATraversal' model [item] -> Int -> model -> JSM model
moveDown optic idx st = do
  popupText @MisoString $ "Moved #" <> inspect (idx + 1) <> " down!"
  pure $ st & cloneTraversal optic %~ swapAt idx (idx + 1)

removeAt :: ATraversal' model [a] -> Int -> model -> JSM model
removeAt optic idx st = do
  popupText @MisoString $ "Removed #" <> inspect (idx + 1) <> "!"
  pure $ st & cloneTraversal optic %~ ((>>= uncurry updater) . zip [0 ..])
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
  model ->
  JSM model
duplicateAt optic idx st = do
  duplicator <- newUniqueDuplicator @MisoString
  let updater loc el =
        if loc == idx
          then [el, closed $ duplicator el]
          else [el]
  popupText @MisoString $ "Duplicated #" <> inspect (idx + 1) <> "!"
  pure $ st & cloneTraversal optic %~ ((>>= uncurry updater) . zip [0 ..])
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

openBrowserPage :: URI -> model -> JSM model
openBrowserPage uri st = do
  void $ JS.global ^. JS.js1 @MisoString "openBrowserPage" (URI.render uri)
  pure st

enterOrEscapeBlur :: Uid -> KeyCode -> model -> JSM model
enterOrEscapeBlur uid (KeyCode code) st = do
  let enterOrEscape = [13, 27] :: [Int]
  when (code `elem` enterOrEscape)
    . void
    . JS.eval @MisoString
    $ "document.getElementById('"
    <> htmlUid uid
    <> "').getElementsByTagName('input')[0].blur();"
  pure st
