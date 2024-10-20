module Functora.Miso.Jsm.Generic
  ( popupText,
    shareText,
    moveUp,
    moveDown,
    removeAt,
    openBrowserPage,
    enterOrEscapeBlur,
    insertStorage,
    selectStorage,
    selectBarcode,
    selectClipboard,
    genericPromise,
    printCurrentPage,
    saveFile,
  )
where

import qualified Data.ByteString.Lazy as BL
import Functora.Miso.Prelude
import qualified GHCJS.Types as JS
import qualified Language.Javascript.JSaddle as JS
import qualified Text.URI as URI
import qualified Prelude ((!!))

--
-- TODO : better code with JS foreign import???
--

getPkg :: JSM JS.JSVal
getPkg =
  JS.global JS.! ("h$miso_widgets" :: Unicode)

popupText :: (Show a, Data a) => a -> JSM ()
popupText x = do
  pkg <- getPkg
  void $ pkg ^. JS.js1 ("popupText" :: Unicode) (inspect x :: Unicode)

shareText :: (Show a, Data a) => a -> model -> JSM model
shareText x st = do
  let txt = inspect @Unicode x
  unless (txt == mempty) $ do
    pkg <- getPkg
    prom <- pkg ^. JS.js1 ("shareText" :: Unicode) txt
    success <- JS.function $ \_ _ _ -> popupText @Unicode "Copied!"
    failure <- JS.function $ \_ _ _ -> popupText @Unicode "Failed to copy!"
    void $ prom ^. JS.js2 ("then" :: Unicode) success failure
  pure st

moveUp :: ATraversal' model [item] -> Int -> model -> JSM model
moveUp optic idx st = do
  popupText @Unicode $ "Moved #" <> inspect (idx + 1) <> " up!"
  pure $ st & cloneTraversal optic %~ swapAt (idx - 1) idx

moveDown :: ATraversal' model [item] -> Int -> model -> JSM model
moveDown optic idx st = do
  popupText @Unicode $ "Moved #" <> inspect (idx + 1) <> " down!"
  pure $ st & cloneTraversal optic %~ swapAt idx (idx + 1)

removeAt :: ATraversal' model [a] -> Int -> model -> JSM model
removeAt optic idx st = do
  popupText @Unicode $ "Removed #" <> inspect (idx + 1) <> "!"
  pure $ st & cloneTraversal optic %~ ((>>= uncurry updater) . zip [0 ..])
  where
    updater loc el =
      if loc == idx
        then mempty
        else [el]

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
  pkg <- getPkg
  void $ pkg ^. JS.js1 @Unicode "openBrowserPage" (URI.render uri)
  pure st

enterOrEscapeBlur :: Uid -> KeyCode -> model -> JSM model
enterOrEscapeBlur uid (KeyCode code) st = do
  let enterOrEscape = [13, 27] :: [Int]
  when (code `elem` enterOrEscape) $ do
    res <-
      either throw pure
        . decodeUtf8Strict @Unicode
        . unTagged
        $ "document.getElementById('"
        <> htmlUid uid
        <> "').getElementsByTagName('input')[0].blur();"
    void
      $ JS.eval res
  pure st

insertStorage :: (ToJSON a) => Unicode -> a -> JSM ()
insertStorage key raw = do
  val <-
    either throw pure
      . decodeUtf8Strict @Unicode @BL.ByteString
      . unTagged
      $ encodeJson raw
  pkg <- getPkg
  void
    $ pkg
    ^. JS.js2 @Unicode "insertStorage" key val

selectStorage :: (FromJSON a) => Unicode -> (Maybe a -> JSM ()) -> JSM ()
selectStorage key after =
  genericPromise @[Unicode] @Unicode "selectStorage" [key] $ \case
    Nothing ->
      after Nothing
    Just str ->
      case decodeJson
        . Tagged @"UTF-8"
        $ encodeUtf8 @Unicode @BL.ByteString str of
        Left e -> do
          consoleLog e
          after Nothing
        Right res ->
          after $ Just res

selectBarcode :: (Maybe Unicode -> JSM ()) -> JSM ()
selectBarcode after =
  genericPromise @[Unicode] @Unicode "selectBarcode" mempty
    $ after
    . fmap strip

selectClipboard :: (Maybe Unicode -> JSM ()) -> JSM ()
selectClipboard after =
  genericPromise @[Unicode] @Unicode "selectClipboard" mempty
    $ after
    . fmap strip

genericPromise ::
  forall args res.
  ( JS.MakeArgs args,
    JS.FromJSVal res
  ) =>
  Unicode ->
  args ->
  (Maybe res -> JSM ()) ->
  JSM ()
genericPromise fun argv after = do
  success <- JS.function $ \_ _ ->
    handleAny (\e -> consoleLog e >> after Nothing) . \case
      [val] -> do
        valExist <- ghcjsPure $ JS.isTruthy val
        if not valExist
          then after Nothing
          else do
            mres <- JS.fromJSVal @res val
            res <- maybe (throwString @String "Failure, bad result!") pure mres
            after $ Just res
      _ ->
        throwString @String "Failure, bad argv!"
  failure <-
    JS.function $ \_ _ e -> do
      msg <- handleAny (\_ -> pure "Unknown") $ JS.valToText e
      consoleLog @Unicode $ "Failure, " <> inspect msg <> "!"
      after Nothing
  pkg <- getPkg
  prom <- pkg ^. JS.jsf fun argv
  void $ prom ^. JS.js2 @Unicode "then" success failure

printCurrentPage :: Unicode -> JSM ()
printCurrentPage name = do
  pkg <- getPkg
  void $ pkg ^. JS.js1 ("printCurrentPage" :: Unicode) name

saveFile :: forall a. (From a [Word8]) => Unicode -> Unicode -> a -> JSM ()
saveFile name mime bs = do
  argv <-
    sequence
      [ JS.toJSVal name,
        JS.toJSVal mime,
        JS.toJSVal $ from @a @[Word8] bs
      ]
  genericPromise @[JS.JSVal] @Unicode "saveFile" argv $ \case
    Nothing -> pure ()
    Just str -> popupText str
