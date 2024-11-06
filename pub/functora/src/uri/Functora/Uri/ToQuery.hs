{-# LANGUAGE CPP #-}

module Functora.Uri.ToQuery
  ( ToQuery (..),
    GToQuery (..),
    genericToQuery,
  )
where

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
  toQuery = genericToQuery

class GToQuery f where
  gToQuery :: f p -> [QueryParam]

genericToQuery :: (Generic a, GToQuery (Rep a)) => a -> [QueryParam]
genericToQuery = gToQuery . G.from

instance (GToQuery a) => GToQuery (M1 D c a) where
  gToQuery (M1 x) = gToQuery x

instance (GToQuery a) => GToQuery (M1 C c a) where
  gToQuery (M1 x) = gToQuery x

instance
  ( Typeable a,
    Selector s,
    ToQueryField a
  ) =>
  GToQuery (M1 S s (K1 i a))
  where
  gToQuery m1@(M1 (K1 a)) = do
    let name = Toml.stripTypeNamePrefix (Proxy @a) $ selName m1
    if null name
      then mempty
      else do
        k <- mkQueryKey $ pack name
        v <- mkQueryValue $ toQueryField a
        pure $ QueryParam k v

instance (GToQuery a, GToQuery b) => GToQuery (a :*: b) where
  gToQuery (a :*: b) = gToQuery a <> gToQuery b

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
