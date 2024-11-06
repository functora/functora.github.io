{-# LANGUAGE CPP #-}

module Functora.Uri.FromQuery
  ( FromQuery (..),
    GFromQuery (..),
    genericFromQuery,
    FromQueryField (..),
    textFromQueryField,
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
#if defined(__GHCJS__) && defined(ghcjs_HOST_OS) && defined(wasi_HOST_OS)
import Data.JSString (JSString)
#endif

class FromQuery a where
  fromQuery ::
    (MonadThrow m) => [QueryParam] -> m a
  default fromQuery ::
    (Generic a, GFromQuery (Rep a), MonadThrow m) => [QueryParam] -> m a
  fromQuery =
    genericFromQuery

class GFromQuery f where
  gFromQuery :: (MonadThrow m) => QueryMap -> m (f p)

genericFromQuery ::
  ( Generic a,
    GFromQuery (Rep a),
    MonadThrow m
  ) =>
  [QueryParam] ->
  m a
genericFromQuery =
  fmap G.to . gFromQuery . toQueryMap

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
    k <-
      mkQueryKey
        . pack
        . Toml.stripTypeNamePrefix (Proxy @a)
        $ selName (error "selName" :: M1 S s (K1 i a) ())
    v <-
      maybe (throw $ FromQueryMissingField k) pure
        $ Map.lookup k params
    fmap (M1 . K1)
      $ fromQueryField k v

instance (GFromQuery a, GFromQuery b) => GFromQuery (a :*: b) where
  gFromQuery params = (:*:) <$> gFromQuery params <*> gFromQuery params

class FromQueryField a where
  fromQueryField ::
    ( MonadThrow m
    ) =>
    RText 'QueryKey ->
    RText 'QueryValue ->
    m a

textFromQueryField ::
  forall a m.
  ( From Text a,
    Applicative m
  ) =>
  RText 'QueryKey ->
  RText 'QueryValue ->
  m a
textFromQueryField _ = pure . from @Text @a . unRText

readFromQueryField ::
  ( Read a,
    MonadThrow m
  ) =>
  RText 'QueryKey ->
  RText 'QueryValue ->
  m a
readFromQueryField k v =
  maybe (throw $ FromQueryInvalidField k v) pure
    . readMaybe
    $ unRText v

instance FromQueryField Text where
  fromQueryField = textFromQueryField

instance FromQueryField String where
  fromQueryField = textFromQueryField

instance FromQueryField Int where
  fromQueryField = readFromQueryField

instance FromQueryField Integer where
  fromQueryField = readFromQueryField

#if defined(__GHCJS__) && defined(ghcjs_HOST_OS) && defined(wasi_HOST_OS)
instance ToQueryField JSString where
  fromQueryField = textFromQueryField
#endif

--
-- Extra stuff
--

data FromQueryException
  = FromQueryMissingField (RText 'QueryKey)
  | FromQueryInvalidField (RText 'QueryKey) (RText 'QueryValue)
  deriving stock (Eq, Ord, Show, Data, Generic)

instance Exception FromQueryException

type QueryMap = Map (RText 'QueryKey) (RText 'QueryValue)

toQueryMap :: [QueryParam] -> QueryMap
toQueryMap =
  foldl
    ( \acc -> \case
        QueryFlag k ->
          Map.insert k (either impureThrow id $ mkQueryValue "True") acc
        QueryParam k v ->
          Map.insert k v acc
    )
    mempty
