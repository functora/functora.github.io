{-# LANGUAGE DataKinds, GADTs, KindSignatures, TemplateHaskell, TypeFamilies #-}

{- | The goal of
the [singlethongs](https://hackage.haskell.org/package/singlethongs) library
is to offer the bare minimum of what
the [singletons](https://hackage.haskell.org/package/singletons) library offers
in a small package that's easy to compile across different GHC versions,
including GHCJS.

This module exports a minimal reproduction of what
the [singletons](https://hackage.haskell.org/package/singletons) package offers.
Namely 'Sing', 'SingI', 'SomeSing' and 'SingKind', as well as @TemplateHaskell@
support for generating the relevant instances for custom types. If there is
some feature that you thing could be added to this library,
please [suggest it](https://gitlab.com/k0001/singlethongs/issues).

The types exported by this module are not the same types as the types as the
one exported by
the [singletons](https://hackage.haskell.org/package/singletons) package.
Even if they have the same names and implementation, they are not seen as
equal by the type-checker. They are only intended to be a drop-in replacement.
-}
module Singlethongs
 ( Sing
 , SingI(..)
 , SomeSing(..)
 , withSomeSing
 , SingKind(..)
   -- * Template Haskell
 , singlethongs
   -- * Re-exports
 , TestEquality(testEquality)
 , (:~:)(Refl)
 ) where

import Singlethongs.Internal
import Singlethongs.TH
import Data.Type.Equality

