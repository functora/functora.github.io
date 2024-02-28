{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module BitfinexClient.Orphan () where

import BitfinexClient.Import.External
import qualified Data.Bifunctor as Bifunctor
import qualified Data.Ratio as Ratio
import qualified Database.Persist as P
import qualified Database.Persist.Sql as P
import qualified Witch
import qualified Witch.From as From
import qualified Witch.TryFrom as TryFrom

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

instance
  (From.From a b, Integral b) =>
  From.From (Ratio.Ratio a) (Ratio.Ratio b)
  where
  from s =
    From.from
      (Ratio.numerator s)
      Ratio.% From.from (Ratio.denominator s)

instance
  forall a b.
  (TryFrom.TryFrom a b, Integral b) =>
  TryFrom.TryFrom (Ratio.Ratio a) (Ratio.Ratio b)
  where
  tryFrom s =
    (Ratio.%)
      <$> tryFrom' (Ratio.numerator s)
      <*> tryFrom' (Ratio.denominator s)
    where
      tryFrom' ::
        a ->
        Either
          ( TryFromException
              (Ratio.Ratio a)
              (Ratio.Ratio b)
          )
          b
      tryFrom' =
        Bifunctor.first (withTarget . withSource s)
          . TryFrom.tryFrom
