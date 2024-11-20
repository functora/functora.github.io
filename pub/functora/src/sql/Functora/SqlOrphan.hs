{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Functora.SqlOrphan () where

import qualified Data.Data as Data
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Database.Esqueleto.Legacy hiding (from)
import Functora.Money
import Functora.Prelude hiding (Key)
import qualified Text.URI as URI

deriving stock instance Data PersistValue

deriving stock instance Data LiteralType

deriving stock instance (Data a, Data (Key a)) => Data (Entity a)

instance PersistField (Ratio Natural) where
  toPersistValue = PersistRational . from @(Ratio Natural) @Rational
  fromPersistValue raw = do
    rat <- fromPersistValue raw
    first (const failure) $ tryFrom @Rational @(Ratio Natural) rat
    where
      failure =
        "Ratio Natural PersistValue is invalid " <> inspect @Text raw

deriving via Rational instance PersistFieldSql (Ratio Natural)

instance PersistField URI where
  toPersistValue = PersistText . URI.render
  fromPersistValue = \case
    PersistText x -> first (const $ failure x) $ URI.mkURI x
    x -> Left $ failure x
    where
      failure x = "URI PersistValue is invalid " <> inspect @Text x

deriving via Text instance PersistFieldSql URI

instance PersistField UUID where
  toPersistValue =
    PersistLiteral_ Escaped . UUID.toASCIIBytes
  fromPersistValue = \case
    PersistLiteral_ Escaped x ->
      maybe
        (Left $ "Failed to deserialize a UUID, got literal: " <> inspect x)
        Right
        $ UUID.fromASCIIBytes x
    failure ->
      Left $ "Failed to deserialize a UUID, got: " <> inspect failure

instance PersistFieldSql UUID where
  sqlType = const $ SqlOther "uuid"

instance (Typeable a) => Data (BackendKey a) where
  gunfold _ z = z . Data.fromConstr
  toConstr _ = error "TODO : toConstr BackendKey"
  dataTypeOf _ = error "TODO : dataTypeOf BackendKey"

instance PersistField Integer where
  toPersistValue raw =
    either
      (const . error $ "Integer is invalid PersistValue " <> inspect @Text raw)
      PersistInt64
      $ tryFrom @Integer @Int64 raw
  fromPersistValue = \case
    PersistInt64 x -> Right $ from @Int64 @Integer x
    raw -> Left $ "PersistValue is invalid Integer " <> inspect raw

deriving via Int64 instance PersistFieldSql Integer

instance (PersistField rep) => PersistField (Tagged tags rep) where
  toPersistValue = toPersistValue . unTagged
  fromPersistValue = fmap Tagged . fromPersistValue

instance (PersistFieldSql rep) => PersistFieldSql (Tagged tags rep) where
  sqlType = const . sqlType $ Proxy @rep

instance (PersistField rep) => PersistField (NonEmpty rep) where
  toPersistValue =
    toPersistValue . toList
  fromPersistValue raw = do
    lst <- fromPersistValue raw
    maybe (Left "Unexpected empty persist value") Right $ nonEmpty lst

instance (PersistFieldSql rep) => PersistFieldSql (NonEmpty rep) where
  sqlType = const . sqlType $ Proxy @[rep]
