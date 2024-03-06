{-# LANGUAGE DataKinds, GADTs, KindSignatures, TemplateHaskell, TypeFamilies,
             ScopedTypeVariables, TypeApplications #-}
{-# OPTIONS_GHC -ddump-splices #-}

module Main (main) where

import Control.Monad (when)

-- We import qualified to test that TH names don't make silly assumptions.
import qualified Singlethongs as S


data A = Ax deriving (Eq)
S.singlethongs ''A
{- =====>
data instance S.Sing (x :: A) where
  SAx :: S.Sing 'Ax

instance S.SingKind A where
  type S.Demote A = A
  fromSing SAx = Ax
  toSing Ax = S.SomeSing SAx

instance S.TestEquality (S.Sing :: A -> Type) where
  testEquality SAx SAx = Just S.Refl

instance S.SingI 'Ax where sing = SAx
-}

t_A :: Bool
t_A = and
  [ S.fromSing (SAx :: S.Sing 'Ax) == (Ax :: S.Demote A)
  , S.withSomeSing @A Ax (\sAx -> S.fromSing sAx == Ax)
  , S.testEquality SAx SAx == Just S.Refl
  ]

data B = Bx | By deriving (Eq)
S.singlethongs ''B
{- =====>

data instance S.Sing (x :: B) where
  SBx :: S.Sing 'Bx
  SBy :: S.Sing 'By

instance S.SingKind B where
  type S.Demote B = B
  fromSing SBx = Bx
  fromSing SBy = By
  toSing Bx = S.SomeSing SBx
  toSing By = S.SomeSing SBy

instance S.TestEquality (S.Sing :: B -> Type) where
  testEquality SBx SBx = Just S.Refl
  testEquality SBy SBy = Just S.Refl
  testEquality _ _ = Nothing

instance S.SingI  'Bx where sing = SBx
instance S.SingI  'By where sing = SBy
-}

t_B :: Bool
t_B = and
  [ S.fromSing (SBx :: S.Sing 'Bx) == (Bx :: S.Demote B)
  , S.withSomeSing @B Bx (\sBx -> S.fromSing sBx == Bx)
  , S.fromSing (SBy :: S.Sing 'By) == (By :: S.Demote B)
  , S.withSomeSing @B By (\sBy -> S.fromSing sBy == By)
  , S.testEquality SBx SBx == Just S.Refl
  , S.testEquality SBx SBy == Nothing
  , S.testEquality SBy SBx == Nothing
  , S.testEquality SBy SBy == Just S.Refl
  ]

main :: IO ()
main = do
  when (not t_A) $ fail "Something wrong with t_A"
  when (not t_B) $ fail "Something wrong with t_B"
