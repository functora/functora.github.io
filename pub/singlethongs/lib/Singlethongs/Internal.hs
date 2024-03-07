{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
#if __GLASGOW_HASKELL__ >= 810
{-# LANGUAGE StandaloneKindSignatures #-}
#endif

module Singlethongs.Internal
  ( Sing,
    SingI (sing),
    SomeSing (SomeSing),
    withSomeSing,
    SingKind (Demote, fromSing, toSing),
    KindOf,
    demote,
  )
where

import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy (..))
import GHC.TypeLits
  ( KnownSymbol,
    SomeSymbol (..),
    Symbol,
    someSymbolVal,
    symbolVal,
  )

#if __GLASGOW_HASKELL__ >= 810
type Sing :: k -> Type
#endif
data family Sing (a :: k)

#if __GLASGOW_HASKELL__ >= 900
type SingI :: forall {k}. k -> Constraint
#endif
class SingI (a :: k) where
  sing :: Sing a

#if __GLASGOW_HASKELL__ >= 810
type SomeSing :: Type -> Type
#endif
data SomeSing k where
  SomeSing :: Sing (a :: k) -> SomeSing k

withSomeSing :: (SingKind k) => Demote k -> (forall (a :: k). Sing a -> r) -> r
withSomeSing x f = case toSing x of SomeSing x' -> f x'
{-# INLINE withSomeSing #-}

#if __GLASGOW_HASKELL__ >= 810
type SingKind :: Type -> Constraint
#endif
class SingKind k where
  type Demote k = (r :: Type) | r -> k
  fromSing :: Sing (a :: k) -> Demote k
  toSing :: Demote k -> SomeSing k

#if __GLASGOW_HASKELL__ >= 810
type KindOf :: k -> Type
#endif
type KindOf (a :: k) = k

#if __GLASGOW_HASKELL__ >= 900
demote :: forall {k} (a :: k). (SingKind k, SingI a) => Demote k
#else
demote :: forall a. (SingKind (KindOf a), SingI a) => Demote (KindOf a)
#endif
demote = fromSing (sing :: Sing a)

--
-- TypeLits
--

data instance Sing (n :: Symbol) where
  SSym :: (KnownSymbol n) => Sing n

instance (KnownSymbol n) => SingI n where
  sing = SSym

instance SingKind Symbol where
  type Demote Symbol = String
  fromSing (SSym :: Sing n) = symbolVal (Proxy :: Proxy n)
  toSing s = case someSymbolVal s of
    SomeSymbol (_ :: Proxy n) -> SomeSing (SSym :: Sing n)
