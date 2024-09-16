{-# LANGUAGE UndecidableInstances #-}

module Functora.Soplate
  ( Soplate (..),
    inspectSop,
  )
where

import qualified Data.ByteString.Lazy as BL
import Functora.Prelude
import Generics.SOP hiding (Generic)
import Generics.SOP.GGP

type MonadTraversal a s = forall f. (Monad f) => (s -> f s) -> a -> f a

class Soplate s a where
  soplate :: MonadTraversal a s

class ChildsOf s a code where
  childsOf :: MonadTraversal a s

instance
  ( GTo a,
    GFrom a,
    Generic a,
    GCode a ~ (xs ': xss),
    All2 (Soplate s) (xs ': xss)
  ) =>
  ChildsOf s a (xs ': xss)
  where
  childsOf f x = gto <$> childsOf_SOP (gfrom x)
    where
      childsOf_SOP = hctraverse (Proxy @(Soplate s)) (soplate f . unI)

instance {-# INCOHERENT #-} ChildsOf s a xs where
  childsOf = const pure

instance (ChildsOf s a (GCode a)) => Soplate s a where
  soplate f x = childsOf @s @a @(GCode a) f x

instance {-# OVERLAPPING #-} (ChildsOf a a (GCode a)) => Soplate a a where
  soplate f x = f =<< childsOf @a @a @(GCode a) f x

inspectSop ::
  forall dst src.
  ( Show src,
    Typeable src,
    Soplate ByteString src,
    Soplate BL.ByteString src,
    Textual dst
  ) =>
  src ->
  dst
inspectSop =
  display @dst @src
    . over soplate prettyByteString
    . over soplate prettyLazyByteString
