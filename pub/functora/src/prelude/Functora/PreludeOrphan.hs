{-# OPTIONS_GHC -Wno-orphans #-}

module Functora.PreludeOrphan () where

import qualified Data.Coerce as Coerce
import qualified Data.Data as Data
import Data.Generics (Data)
import Data.Ratio
import Data.Scientific (Scientific)
import Data.Tagged
import Universum
import Witch.Mini

instance Data SomeException where
  gunfold _ z = z . Data.fromConstr
  toConstr _ = error "TODO : toConstr SomeException"
  dataTypeOf _ = error "TODO : dataTypeOf SomeException"

deriving stock instance (Data a, Data b) => Data (TryFromException a b)

deriving stock instance
  ( Typeable kind,
    Typeable (tags :: kind),
    Data rep
  ) =>
  Data (Tagged tags rep)

instance
  ( From source target,
    Integral target
  ) =>
  From (Ratio source) (Ratio target)
  where
  from x =
    from @source @target (numerator x)
      % from @source @target (denominator x)

instance
  forall a.
  ( TryFrom Integer a,
    Integral a
  ) =>
  TryFrom Scientific (Ratio a)
  where
  tryFrom x =
    first (withTarget . withSource x)
      . tryFrom @Rational @(Ratio a)
      $ toRational x

instance
  forall a b.
  ( TryFrom a b,
    Integral b
  ) =>
  TryFrom (Ratio a) (Ratio b)
  where
  tryFrom s =
    (%)
      <$> tryFrom' (numerator s)
      <*> tryFrom' (denominator s)
    where
      tryFrom' :: a -> Either (TryFromException (Ratio a) (Ratio b)) b
      tryFrom' = first (withTarget . withSource s) . tryFrom @a @b

withSource ::
  source2 ->
  TryFromException source1 t ->
  TryFromException source2 t
withSource s (TryFromException _ e) =
  TryFromException s e

withTarget ::
  forall target2 source target1.
  TryFromException source target1 ->
  TryFromException source target2
withTarget =
  Coerce.coerce
