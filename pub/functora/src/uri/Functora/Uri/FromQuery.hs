{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}

module Functora.Uri.FromQuery
  ( FromQuery (..),
    GFromQuery (..),
    genericFromQuery,
    FromQueryField (..),
    castFromQueryField,
    readFromQueryField,
    FromQueryException (..),
  )
where

import qualified Data.Map as Map
import Functora.Prelude
import GHC.Generics hiding (from)
import qualified GHC.Generics as G
import Text.URI
import qualified Toml
#if defined(__GHCJS__) || defined(ghcjs_HOST_OS) || defined(wasi_HOST_OS)
import Data.JSString (JSString)
#endif

class FromQuery a where
  fromQuery :: [QueryParam] -> Either FromQueryException a
  default fromQuery ::
    ( Generic a,
      GFromQuery (Rep a)
    ) =>
    [QueryParam] ->
    Either FromQueryException a
  fromQuery =
    genericFromQuery

instance
  ( Generic a,
    GFromQuery (Rep a)
  ) =>
  FromQuery (GenericType a)
  where
  fromQuery = fmap GenericType . genericFromQuery

class GFromQuery f where
  gFromQuery :: QueryMap -> Either FromQueryException (f p)

genericFromQuery ::
  ( Generic a,
    GFromQuery (Rep a)
  ) =>
  [QueryParam] ->
  Either FromQueryException a
genericFromQuery =
  fmap G.to . gFromQuery . newQueryMap

instance (GFromQuery a) => GFromQuery (M1 D c a) where
  gFromQuery params = M1 <$> gFromQuery params

instance (GFromQuery a) => GFromQuery (M1 C c a) where
  gFromQuery params = M1 <$> gFromQuery params

instance
  ( Typeable a,
    Selector s,
    FromQueryField a
  ) =>
  GFromQuery (M1 S s (K1 i a))
  where
  gFromQuery params = do
    let rawKey =
          pack
            . Toml.stripTypeNamePrefix (Proxy @a)
            $ selName (error "selName" :: M1 S s (K1 i a) ())
    k <-
      either (const . Left $ FromQueryInvalidKey rawKey) pure
        $ mkQueryKey rawKey
    v <-
      maybe (Left $ FromQueryMissingField k) pure
        $ Map.lookup k params
    second (M1 . K1)
      $ fromQueryField k v

instance (GFromQuery a, GFromQuery b) => GFromQuery (a :*: b) where
  gFromQuery params = (:*:) <$> gFromQuery params <*> gFromQuery params

class FromQueryField a where
  fromQueryField ::
    RText 'QueryKey ->
    RText 'QueryValue ->
    Either FromQueryException a

castFromQueryField ::
  forall a.
  ( From Text a
  ) =>
  RText 'QueryKey ->
  RText 'QueryValue ->
  Either FromQueryException a
castFromQueryField _ = pure . from @Text @a . unRText

readFromQueryField ::
  ( Read a
  ) =>
  RText 'QueryKey ->
  RText 'QueryValue ->
  Either FromQueryException a
readFromQueryField k v =
  maybe (Left $ FromQueryInvalidField k v) pure
    . readMaybe
    $ unRText v

instance (Read a) => FromQueryField (GenericEnum a) where
  fromQueryField k = fmap GenericEnum . readFromQueryField k

deriving via (GenericEnum Bool) instance FromQueryField Bool

instance FromQueryField Text where
  fromQueryField = castFromQueryField

instance FromQueryField String where
  fromQueryField = castFromQueryField

instance FromQueryField Int where
  fromQueryField = readFromQueryField

instance FromQueryField Integer where
  fromQueryField = readFromQueryField

#if defined(__GHCJS__) || defined(ghcjs_HOST_OS) || defined(wasi_HOST_OS)
instance FromQueryField JSString where
  fromQueryField = castFromQueryField
#endif

--
-- Extra stuff
--

data FromQueryException
  = FromQueryInvalidKey Text
  | FromQueryMissingField (RText 'QueryKey)
  | FromQueryInvalidField (RText 'QueryKey) (RText 'QueryValue)
  deriving stock (Eq, Ord, Show, Data, Generic)

instance Exception FromQueryException

type QueryMap = Map (RText 'QueryKey) (RText 'QueryValue)

newQueryMap :: [QueryParam] -> QueryMap
newQueryMap =
  foldl
    ( \acc -> \case
        QueryFlag k ->
          Map.insert k (either impureThrow id $ mkQueryValue "True") acc
        QueryParam k v ->
          Map.insert k v acc
    )
    mempty
