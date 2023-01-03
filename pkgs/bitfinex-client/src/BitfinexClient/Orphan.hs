{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module BitfinexClient.Orphan () where

import BitfinexClient.Import.External
import qualified Database.Persist as P
import qualified Database.Persist.Sql as P
import qualified Witch

instance
  (P.ToBackendKey P.SqlBackend a) =>
  TryFrom (P.Key a) Natural
  where
  tryFrom =
    tryFrom `composeTryLhs` P.fromSqlKey

instance
  (P.ToBackendKey P.SqlBackend a) =>
  TryFrom Natural (P.Key a)
  where
  tryFrom =
    P.toSqlKey `composeTryRhs` tryFrom
