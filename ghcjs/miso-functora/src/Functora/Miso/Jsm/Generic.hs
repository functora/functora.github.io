module Functora.Miso.Jsm.Generic
  ( popupText,
    shareText,
    moveUp,
    moveDown,
    removeAt,
    openBrowserPage,
    enterOrEscapeBlur,
    blur,
    insertStorage,
    selectStorage,
    selectBarcode,
    selectClipboard,
    selectFile,
    genericPromise,
    printCurrentPage,
    saveFile,
    fetchUrlAsRfc2397,
  )
where

import qualified Data.ByteString.Lazy as BL
import Functora.Miso.Prelude
import Functora.Miso.Types
import qualified GHCJS.Buffer as Buf
import qualified GHCJS.Types as JS
import qualified JavaScript.TypedArray.ArrayBuffer as AB
import qualified Language.Javascript.JSaddle as JS
import qualified Text.URI as URI
import qualified Prelude ((!!))

--
-- TODO : better code with JS foreign import???
--

getPkg :: JSM JS.JSVal
getPkg =
  JS.global JS.! ("h$miso_functora" :: Unicode)

popupText :: (Show a, Data a) => a -> JSM ()
popupText x = do
  pkg <- getPkg
  void $ pkg ^. JS.js1 ("popupText" :: Unicode) (inspect x :: Unicode)

shareText :: (Show a, Data a) => a -> Update model
shareText x =
  EffectUpdate $ do
    let txt = inspect @Unicode x
    unless (txt == mempty) $ do
      pkg <- getPkg
      prom <- pkg ^. JS.js1 ("shareText" :: Unicode) txt
      success <- JS.function $ \_ _ _ -> popupText @Unicode "Copied!"
      failure <- JS.function $ \_ _ _ -> popupText @Unicode "Failed to copy!"
      void $ prom ^. JS.js2 ("then" :: Unicode) success failure

moveUp :: ATraversal' model [item] -> Int -> Update model
moveUp optic idx =
  PureAndImpureUpdate
    ( cloneTraversal optic %~ swapAt (idx - 1) idx
    )
    ( do
        popupText @Unicode $ "Moved #" <> inspect (idx + 1) <> " up!"
        pure id
    )

moveDown :: ATraversal' model [item] -> Int -> Update model
moveDown optic idx =
  PureAndImpureUpdate
    ( cloneTraversal optic %~ swapAt idx (idx + 1)
    )
    ( do
        popupText @Unicode $ "Moved #" <> inspect (idx + 1) <> " down!"
        pure id
    )

removeAt :: ATraversal' model [a] -> Int -> Update model
removeAt optic idx =
  PureAndImpureUpdate
    ( cloneTraversal optic %~ ((>>= uncurry updater) . zip [0 ..])
    )
    ( do
        popupText @Unicode $ "Removed #" <> inspect (idx + 1) <> "!"
        pure id
    )
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

openBrowserPage :: URI -> Update model
openBrowserPage uri =
  EffectUpdate $ do
    pkg <- getPkg
    void $ pkg ^. JS.js1 @Unicode "openBrowserPage" (URI.render uri)

enterOrEscapeBlur :: Uid -> KeyCode -> Update model
enterOrEscapeBlur uid (KeyCode code) =
  EffectUpdate $ do
    let enterOrEscape = [13, 27] :: [Int]
    when (code `elem` enterOrEscape) $ blur uid

blur :: Uid -> JSM ()
blur uid = do
  el <-
    getElementById
      . either impureThrow id
      . decodeUtf8Strict
      . unTagged
      $ htmlUid uid
  is <- ghcjsPure $ JS.isTruthy el
  when is . void $ el ^. JS.js0 @Unicode "blur"

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

selectFile :: JS.JSVal -> (Maybe Unicode -> JSM ()) -> JSM ()
selectFile file after =
  genericPromise @[JS.JSVal] @Unicode "selectFile" [file]
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

saveFile :: Unicode -> Unicode -> ByteString -> JSM ()
saveFile name mime bs = do
  (buf, off, len) <- ghcjsPure $ Buf.fromByteString bs
  ab0 <- ghcjsPure . JS.jsval_ =<< ghcjsPure (Buf.getArrayBuffer buf)
  ab1 <- ab0 ^. JS.jsf ("slice" :: Unicode) ([off, off + len] :: [Int])
  argv <-
    sequence
      [ JS.toJSVal name,
        JS.toJSVal mime,
        JS.toJSVal ab1
      ]
  genericPromise @[JS.JSVal] @Unicode "saveFile" argv $ \case
    Nothing -> pure ()
    Just str -> popupText str

fetchUrlAsRfc2397 :: Unicode -> (Maybe ByteString -> JSM ()) -> JSM ()
fetchUrlAsRfc2397 url after = do
  success <- JS.function $ \_ _ ->
    handleAny (\e -> consoleLog e >> after Nothing) . \case
      [val] -> do
        valExist <- ghcjsPure $ JS.isTruthy val
        if not valExist
          then after Nothing
          else do
            ab <- AB.freeze $ JS.pFromJSVal val
            buf <- ghcjsPure $ Buf.createFromArrayBuffer ab
            res <- ghcjsPure $ Buf.toByteString 0 Nothing buf
            after $ Just res
      _ ->
        throwString @String "Failure, bad argv!"
  failure <-
    JS.function $ \_ _ e -> do
      msg <- handleAny (\_ -> pure "Unknown") $ JS.valToText e
      consoleLog @Unicode $ "Failure, " <> inspect msg <> "!"
      after Nothing
  pkg <- getPkg
  prom <- pkg ^. JS.jsf ("fetchUrlAsRfc2397" :: Unicode) ([url] :: [Unicode])
  void $ prom ^. JS.js2 @Unicode "then" success failure
