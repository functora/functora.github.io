{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module App.Prelude (module X, String, Text) where

#if defined(__GHCJS__) || defined(ghcjs_HOST_OS) || defined(wasi_HOST_OS)
import qualified Data.Binary as Binary (get, put)
import qualified Functora.Prelude as Prelude
#endif

import Functora.Cfg
import Functora.Prelude as X hiding
  ( Field (..),
    String,
    Text,
    field,
  )
import Miso.String as X
  ( FromMisoString (..),
    MisoString,
    ToMisoString (..),
    fromMisoString,
    ms,
  )

type String = MisoString

type Text = MisoString

#if defined(__GHCJS__) || defined(ghcjs_HOST_OS) || defined(wasi_HOST_OS)
instance Binary MisoString where
  put = Binary.put . fromMisoString @Prelude.Text
  get = fmap (ms @Prelude.Text) Binary.get

instance From Prelude.Text MisoString where
  from = ms

instance From MisoString Prelude.Text where
  from = fromMisoString

instance From Prelude.String MisoString where
  from = ms

instance From MisoString Prelude.String where
  from = fromMisoString

instance ConvertUtf8 MisoString ByteString where
  encodeUtf8 = encodeUtf8 . from @MisoString @Prelude.Text
  decodeUtf8 = from @Prelude.Text @MisoString . decodeUtf8
  decodeUtf8Strict = fmap (from @Prelude.Text @MisoString) . decodeUtf8Strict
#endif