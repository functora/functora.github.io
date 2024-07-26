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
#endif

inspect :: (Show a, Data a) => a -> MisoString
inspect x =
  case typeOf x `eqTypeRep` typeRep @MisoString of
    Just HRefl -> x
    Nothing -> toMisoString $ Prelude.inspect @Prelude.Text x

consoleLog :: (Show a, Data a) => a -> JSM ()
consoleLog = Miso.consoleLog . inspect
