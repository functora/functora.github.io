{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Functora.Miso.Prelude
  ( module X,
    inspect,
    consoleLog,
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
  ( FromMisoString (..),
    MisoString,
    ToMisoString (..),
    fromMisoString,
  )
import Type.Reflection

#if defined(__GHCJS__) || defined(ghcjs_HOST_OS) || defined(wasi_HOST_OS)
instance Binary MisoString where
  put = Binary.put . fromMisoString @Prelude.Text
  get = fmap (toMisoString @Prelude.Text) Binary.get

instance ConvertUtf8 MisoString ByteString where
  encodeUtf8 = encodeUtf8 . fromMisoString @Prelude.Text
  decodeUtf8 = toMisoString @Prelude.Text . decodeUtf8
  decodeUtf8Strict = fmap (toMisoString @Prelude.Text) . decodeUtf8Strict

instance ToJSONKey MisoString where
  toJSONKey = contramap (fromMisoString @Prelude.Text) $ toJSONKey

instance FromJSONKey MisoString where
  fromJSONKey = fmap toMisoString $ fromJSONKey @Prelude.Text
#endif

instance (ToMisoString a) => ToMisoString (Tagged "UTF-8" a) where
  toMisoString = toMisoString . unTagged

instance (FromMisoString a) => FromMisoString (Tagged "UTF-8" a) where
  fromMisoStringEither = Right . Tagged @"UTF-8" . fromMisoString

inspect :: (Show a, Data a) => a -> MisoString
inspect x =
  case typeOf x `eqTypeRep` typeRep @MisoString of
    Just HRefl -> x
    Nothing -> toMisoString $ Prelude.inspect @Prelude.Text x

consoleLog :: (Show a, Data a) => a -> JSM ()
consoleLog = Miso.consoleLog . inspect
