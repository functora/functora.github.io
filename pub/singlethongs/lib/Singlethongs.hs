{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- | The goal of
-- the [singlethongs](https://hackage.haskell.org/package/singlethongs) library
-- is to offer the bare minimum of what
-- the [singletons](https://hackage.haskell.org/package/singletons) library offers
-- in a small package that's easy to compile across different GHC versions,
-- including GHCJS.
--
-- This module exports a minimal reproduction of what
-- the [singletons](https://hackage.haskell.org/package/singletons) package offers.
-- Namely 'Sing', 'SingI', 'SomeSing' and 'SingKind', as well as @TemplateHaskell@
-- support for generating the relevant instances for custom types. If there is
-- some feature that you thing could be added to this library,
-- please [suggest it](https://gitlab.com/k0001/singlethongs/issues).
--
-- The types exported by this module are not the same types as the types as the
-- one exported by
-- the [singletons](https://hackage.haskell.org/package/singletons) package.
-- Even if they have the same names and implementation, they are not seen as
-- equal by the type-checker. They are only intended to be a drop-in replacement.
module Singlethongs
  ( Sing,
    SingI (..),
    SomeSing (..),
    withSomeSing,
    SingKind (..),
    KindOf,
    demote,

    -- * Template Haskell
    mkSing,
    mkEnum,
    singlethongs,

    -- * Re-exports
    TestEquality (testEquality),
    (:~:) (Refl),
  )
where

import Data.Data (Data)
import Data.Type.Equality
import GHC.Generics (Generic)
import Language.Haskell.TH (Dec, Name, Q)
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH
import Singlethongs.Internal
import Singlethongs.TH

mkSing :: Name -> Q [Dec]
mkSing name =
  mappend
    <$> mkEnum name
    <*> singlethongs name

mkEnum :: Name -> Q [Dec]
mkEnum name = do
  let typ = TH.conT name
  [d|
    deriving stock instance Eq $typ

    deriving stock instance Ord $typ

    deriving stock instance Show $typ

    deriving stock instance Read $typ

    deriving stock instance Enum $typ

    deriving stock instance Bounded $typ

    deriving stock instance Data $typ

    deriving stock instance Generic $typ

    deriving stock instance TH.Lift $typ
    |]
