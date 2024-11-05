{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}

module Functora.Uri.ToQuery (ToQuery (..)) where

import Functora.Prelude
import GHC.Generics hiding (from)
import qualified GHC.Generics as G
import Text.URI
import qualified Toml
#if defined(__GHCJS__) && defined(ghcjs_HOST_OS) && defined(wasi_HOST_OS)
import Data.JSString (JSString)
#endif

class (Typeable a) => ToQuery a where
  toQuery :: a -> [QueryParam]
  default toQuery :: (Generic a, GToQuery (Rep a)) => a -> [QueryParam]
  toQuery = gToQuery (Toml.stripTypeNamePrefix $ Proxy @a) . G.from

class GToQuery f where
  gToQuery :: (String -> String) -> f p -> [QueryParam]

-- Handle datatype metadata
instance (GToQuery a) => GToQuery (M1 D c a) where
  gToQuery fmt (M1 x) = gToQuery fmt x

-- Handle constructor metadata
instance (GToQuery a) => GToQuery (M1 C c a) where
  gToQuery fmt (M1 x) = gToQuery fmt x

instance (Selector s, ToQueryField a) => GToQuery (M1 S s (K1 i a)) where
  gToQuery fmt m1@(M1 (K1 a)) = do
    let name = selName m1
    if null name
      then mempty -- Skip if there is no field name (like unnamed tuples)
      else do
        k <- mkQueryKey . pack $ fmt name
        v <- mkQueryValue $ toQueryField a
        pure $ QueryParam k v

-- Handle product type
instance (GToQuery a, GToQuery b) => GToQuery (a :*: b) where
  gToQuery fmt (a :*: b) = gToQuery fmt a ++ gToQuery fmt b

class ToQueryField a where
  toQueryField :: a -> Text

instance ToQueryField Text where
  toQueryField = id

instance ToQueryField String where
  toQueryField = from @String @Text

instance ToQueryField Int where
  toQueryField = inspect

instance ToQueryField Integer where
  toQueryField = inspect

#if defined(__GHCJS__) && defined(ghcjs_HOST_OS) && defined(wasi_HOST_OS)
instance ToQueryField JSString where
  toQueryField = from @JSString @Text
#endif
