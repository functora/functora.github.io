{-# LANGUAGE UndecidableInstances #-}

module Functora.Soplate
  ( Soplate (..),
  )
where

import Generics.SOP hiding (Generic)
import Generics.SOP.GGP
import Universum hiding (All)

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
