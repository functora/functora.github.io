{-# LANGUAGE ConstraintKinds          #-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE ExplicitNamespaces       #-}
{-# LANGUAGE KindSignatures           #-}
{-# LANGUAGE NoImplicitPrelude        #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE Safe                     #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeOperators            #-}

-- | Type operators for writing convenient type signatures.

module Universum.TypeOps
       ( type Each
       , type With
       , type ($)
       ) where

import Data.Kind (Constraint)

-- | Infix application.
--
-- @
-- f :: Either String $ Maybe Int
-- =
-- f :: Either String (Maybe Int)
-- @
type ($) :: (k2 -> k1) -> k2 -> k1
type f $ a = f a
infixr 2 $

-- | Map several constraints over a single variable.
--
-- @
-- a :: [Show, Read] \<+> a => a -> a
-- =
-- a :: (Show a, Read a) => a -> a
-- @
type family (<+>) (c :: [k -> Constraint]) (a :: k) where
    (<+>) '[] _ = (() :: Constraint)
    (<+>) (ch ': ct) a = (ch a, (<+>) ct a)
infixl 9 <+>

-- | Map several constraints over several variables.
--
-- @
-- f :: Each [Show, Read] [a, b] => a -> b -> String
-- =
-- f :: (Show a, Show b, Read a, Read b) => a -> b -> String
-- @
--
-- To specify list with single constraint / variable, don't forget to prefix
-- it with @\'@:
--
-- @
-- f :: Each '[Show] [a, b] => a -> b -> String
-- @
type family Each (c :: [k -> Constraint]) (as :: [k]) where
    Each _ '[] = (() :: Constraint)
    Each c (h ': t) = (c <+> h, Each c t)

-- | Map several constraints over a single variable.
-- Note, that @With a b ≡ Each a '[b]@
--
-- @
-- a :: With [Show, Read] a => a -> a
-- =
-- a :: (Show a, Read a) => a -> a
-- @
type With :: [k -> Constraint] -> k -> Constraint
type With a b = a <+> b
