{-# LANGUAGE UndecidableInstances #-}

module Functora.Soplate
  ( biplated,
  )
where

import Generics.SOP hiding (Generic)
import Generics.SOP.GGP
import Universum hiding (All)

type MonadTraversal a s = forall f. (Monad f) => (s -> f s) -> a -> f a

class Biplated s a where
  biplated :: MonadTraversal a s

class Childs s code where
  childsOf_SOP :: MonadTraversal (SOP I code) s

instance Childs s '[] where
  childsOf_SOP _ = pure

instance
  ( All2 (Biplated s) (xs : xss),
    Childs s xss
  ) =>
  Childs s (xs : xss)
  where
  childsOf_SOP f (SOP (Z np)) = SOP . Z <$> traverseP f np
  childsOf_SOP f (SOP (S sp)) = SOP . S . unSOP <$> childsOf_SOP f (SOP sp)

class ChildsOf s a code where
  childsOf :: MonadTraversal a s

instance
  ( GFrom a,
    GTo a,
    Generic a,
    Childs s (xs : xss),
    GCode a ~ (xs : xss)
  ) =>
  ChildsOf s a (xs : xss)
  where
  childsOf f x = gto <$> childsOf_SOP f (gfrom x)

instance {-# INCOHERENT #-} ChildsOf s a xs where
  childsOf _ = pure

traverseP :: (All (Biplated s) xs) => MonadTraversal (NP I xs) s
traverseP _ Nil = pure Nil
traverseP f (I x :* xs) = liftA2 (:*) (I <$> biplated f x) (traverseP f xs)

instance (ChildsOf s a (GCode a)) => Biplated s a where
  biplated f x = childsOf @s @a @(GCode a) f x

instance {-# OVERLAPPING #-} (ChildsOf a a (GCode a)) => Biplated a a where
  biplated f x = f =<< childsOf @a @a @(GCode a) f x
