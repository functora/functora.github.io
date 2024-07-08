{-# LANGUAGE Unsafe #-}

{- | Unsafe functions to work with lists and 'Maybe'.
Sometimes unavoidable but better don't use them. This module
is intended to be imported qualified and it's not even included
in default prelude exports.

@
import qualified Universum.Unsafe as Unsafe

foo :: [a] -> a
foo = Unsafe.head
@

-}

module Universum.Unsafe
       ( head
       , tail
       , init
       , last
       , at
       , (!!)
       , fromJust
       , foldr1
       , foldl1
       , minimum
       , maximum
       , minimumBy
       , maximumBy
       ) where

import Data.List (foldl1, foldr1, head, init, last, maximum, maximumBy, minimum, minimumBy, tail,
                  (!!))
import Data.Maybe (fromJust)

import Universum.Base (Int)

-- | Similar to '!!' but with flipped arguments.
{-@ at :: n : Nat -> {xs : NonEmptyList a | len xs > n} -> a @-}
at :: Int -> [a] -> a
at n xs = xs !! n
