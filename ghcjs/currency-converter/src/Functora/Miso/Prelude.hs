{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Functora.Miso.Prelude
  ( module X,
    inspect,
    consoleLog,
    textPopup,
    shareText,
    moveUp,
    moveDown,
    removeAt,
  )
where

#if defined(__GHCJS__) || defined(ghcjs_HOST_OS) || defined(wasi_HOST_OS)
import qualified Data.Binary as Binary (get, put)
#endif
import Functora.Cfg as X
import Functora.Prelude as X hiding
  ( Field (..),
    String,
    Text,
    field,
    inspect,
  )
import qualified Functora.Prelude as Prelude
import qualified Language.Javascript.JSaddle as JS
import Miso as X hiding
  ( Key,
    Text,
    URI,
    at,
    consoleLog,
    for_,
    view,
  )
import qualified Miso
import Miso.String as X
  ( FromMisoString,
    MisoString,
    ToMisoString,
    fromMisoString,
    toMisoString,
  )
import Type.Reflection
import qualified Prelude ((!!))

#if defined(__GHCJS__) || defined(ghcjs_HOST_OS) || defined(wasi_HOST_OS)
instance Binary MisoString where
  put = Binary.put . fromMisoString @Prelude.Text
  get = fmap (toMisoString @Prelude.Text) Binary.get

instance From Prelude.Text MisoString where
  from = toMisoString

instance From Prelude.String MisoString where
  from = toMisoString

instance From MisoString Prelude.Text where
  from = fromMisoString


instance From MisoString Prelude.String where
  from = fromMisoString

instance ConvertUtf8 MisoString ByteString where
  encodeUtf8 = encodeUtf8 . from @MisoString @Prelude.Text
  decodeUtf8 = from @Prelude.Text @MisoString . decodeUtf8
  decodeUtf8Strict = fmap (from @Prelude.Text @MisoString) . decodeUtf8Strict

instance ToJSONKey MisoString where
  toJSONKey = contramap (fromMisoString @Prelude.Text) $ toJSONKey

instance FromJSONKey MisoString where
  fromJSONKey = fmap toMisoString $ fromJSONKey @Prelude.Text
#endif

inspect :: (Show a, Data a) => a -> MisoString
inspect x =
  case typeOf x `eqTypeRep` typeRep @MisoString of
    Just HRefl -> x
    Nothing -> toMisoString $ Prelude.inspect @Prelude.Text x

consoleLog :: (Show a, Data a) => a -> JSM ()
consoleLog = Miso.consoleLog . inspect

--
-- TODO : model-independent textPopup
--
textPopup :: (Show a, Data a) => a -> JSM ()
textPopup = consoleLog

shareText :: (Show a, Data a) => a -> JSM (model -> model)
shareText x = do
  let txt = inspect x
  unless (txt == mempty) $ do
    prom <- JS.global ^. JS.js1 ("shareText" :: MisoString) txt
    success <- JS.function $ \_ _ _ -> textPopup @MisoString "Copied!"
    failure <- JS.function $ \_ _ _ -> textPopup @MisoString "Failed to copy!"
    void $ prom ^. JS.js2 ("then" :: MisoString) success failure
  pure id

moveUp :: ATraversal' model [item] -> Int -> JSM (model -> model)
moveUp optic idx = do
  textPopup @MisoString $ "Moved #" <> inspect (idx + 1) <> " up!"
  pure (& cloneTraversal optic %~ swapAt (idx - 1) idx)

moveDown :: ATraversal' model [item] -> Int -> JSM (model -> model)
moveDown optic idx = do
  textPopup @MisoString $ "Moved #" <> inspect (idx + 1) <> " down!"
  pure (& cloneTraversal optic %~ swapAt idx (idx + 1))

removeAt :: ATraversal' model [a] -> Int -> JSM (model -> model)
removeAt optic idx = do
  textPopup @MisoString $ "Removed #" <> inspect (idx + 1) <> "!"
  pure (& cloneTraversal optic %~ ((>>= uncurry updater) . zip [0 ..]))
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
