{-# OPTIONS_HADDOCK show-extensions #-}

module Bfx.Util
  ( showPercent,
    displaySats,
    displayRational,
    eradicateNull,
  )
where

import Bfx.Import.External
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as A
import qualified Data.Fixed as F
import qualified Data.Vector as V
import qualified Prelude

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
