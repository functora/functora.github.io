{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module Bfx.Util
  ( showType,
    showPercent,
    displaySats,
    displayRational,
    eradicateNull,
    readVia,
    tryReadVia,
    readViaRatio,
    tryReadViaRatio,
  )
where

import Bfx.Import.External
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as A
import qualified Data.Fixed as F
import qualified Data.Text as T
import qualified Data.Text.Read as T
import Data.Typeable (typeRep)
import qualified Data.Vector as V
import qualified Prelude

showType :: forall a b. (Typeable a, IsString b) => b
showType =
  show . typeRep $ Proxy @a

showPercent :: (IsString a, Semigroup a) => Rational -> a
showPercent x =
  fromString
    ( F.showFixed
        True
        ( fromRational
            $ x
            * 100 ::
            F.Fixed F.E2
        )
    )
    <> "%"

displaySats :: (IsString a) => Rational -> a
displaySats =
  displayRational 8

displayRational :: (IsString a) => Int -> Rational -> a
displayRational len rat =
  fromString
    $ (if num < 0 then "-" else "")
    ++ Prelude.shows d ("." ++ right)
  where
    right = case take len (go next) of
      "" -> "0"
      x -> x
    (d, next) = abs num `quotRem` den
    num = numerator rat
    den = denominator rat
    go 0 = ""
    go x =
      let (d', next') = (10 * x) `quotRem` den
       in Prelude.shows d' (go next')

eradicateNull :: A.Value -> A.Value
eradicateNull = \case
  A.Object xs -> A.Object $ A.mapMaybe devastateNull xs
  A.Array xs -> A.Array $ V.mapMaybe devastateNull xs
  x -> x
  where
    devastateNull =
      \case
        A.Null -> Nothing
        x -> Just $ eradicateNull x

readVia ::
  forall through target source.
  ( Read through,
    From through target,
    ToString source,
    'False ~ (source == through),
    'False ~ (through == target)
  ) =>
  source ->
  Either (TryFromException source target) target
readVia x0 =
  case readMaybe
    . T.unpack
    . T.strip
    . fromString
    $ toString x0 of
    Nothing -> Left $ TryFromException x0 Nothing
    Just x -> Right $ from @through x

tryReadVia ::
  forall through target source.
  ( Read through,
    TryFrom through target,
    ToString source,
    'False ~ (source == through),
    'False ~ (through == target)
  ) =>
  source ->
  Either (TryFromException source target) target
tryReadVia x0 =
  case readMaybe
    . T.unpack
    . T.strip
    . fromString
    $ toString x0 of
    Nothing -> Left $ TryFromException x0 Nothing
    Just x -> first (withSource x0) $ tryFrom @through x

readViaRatio ::
  forall through target source.
  ( Fractional through,
    From through target,
    ToString source,
    'False ~ (source == through),
    'False ~ (through == target)
  ) =>
  source ->
  Either (TryFromException source target) target
readViaRatio x0 =
  case T.rational . T.strip . fromString $ toString x0 of
    Right (x, "") -> Right $ from @through x
    Right {} -> failure
    Left {} -> failure
  where
    failure = Left $ TryFromException x0 Nothing

tryReadViaRatio ::
  forall through target source.
  ( Fractional through,
    TryFrom through target,
    ToString source,
    'False ~ (source == through),
    'False ~ (through == target)
  ) =>
  source ->
  Either (TryFromException source target) target
tryReadViaRatio x0 =
  case T.rational . T.strip . T.pack $ toString x0 of
    Right (x, "") -> first (withSource x0) $ tryFrom @through x
    Right {} -> failure
    Left {} -> failure
  where
    failure = Left $ TryFromException x0 Nothing
