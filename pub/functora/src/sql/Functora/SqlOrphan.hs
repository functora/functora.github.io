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
  fromPersistValue = \case
    PersistRational x ->
      first (const $ failure x) $ tryFrom @Rational @(Ratio Natural) x
    x ->
      Left $ failure x
    where
      failure x =
        "Ratio Natural PersistValue is invalid " <> inspect @Text x

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

instance
  ( MoneyTags tags,
    Ratio (IntRep tags) ~ a,
    HasTag (sig :: SignedOrUnsigned) tags
  ) =>
  PersistField (Tagged tags a)
  where
  toPersistValue money =
    let rep = unMoney money
     in case sing :: Sing sig of
          SSigned -> PersistRational rep
          SUnsigned -> PersistRational $ from @(Ratio Natural) @Rational rep
  fromPersistValue raw =
    case raw of
      PersistRational x ->
        case sing :: Sing sig of
          SSigned ->
            pure $ newMoney x
          SUnsigned ->
            bimap (const failure) newMoney $
              tryFrom @Rational @(Ratio Natural) x
      _ ->
        Left failure
    where
      failure =
        inspectType @(Money tags)
          <> " PersistValue is invalid "
          <> inspect raw

deriving via
  Rational
  instance
    ( MoneyTags tags,
      Ratio (IntRep tags) ~ a,
      HasTag (sig :: SignedOrUnsigned) tags
    ) =>
    PersistFieldSql (Tagged tags a)
