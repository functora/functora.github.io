{-# LANGUAGE CPP, LambdaCase, TemplateHaskell #-}
module Singlethongs.TH (singlethongs) where

import Data.Type.Equality
import Language.Haskell.TH
import Singlethongs.Internal

{-| Generate 'Sing', 'SingI', 'SingKind' and 'TestEquality' instances for a
datatype.

Given a datatype like @Foo@ below, having one or more unary constructors:

@
data Foo = Bar | Qux

'singlethongs' ''Foo
@

The following code will be generated:

@
data instance 'Sing' (x :: Foo) where
  SBar :: 'Sing' 'Bar
  SQux :: 'Sing' 'Qux

instance 'SingKind' Fobo where
  type 'Demote' Foo = Foo
  'fromSing' SBar = Bar
  'fromSing' SQux = Qux
  'toSing' Bar = 'SomeSing' SBar
  'toSing' Qux = 'SomeSing' SQux

instance 'SingI' 'Bar where 'sing' = SBar
instance 'SingI' 'Qux where 'sing' = SQux

instance 'TestEquality' ('Sing' :: Foo -> 'Type') where
  'testEquality' SBar SBar = 'Just' 'Refl'
  'testEquality' SQux SQux = 'Just' 'Refl'
  'testEquality' _ _ = 'Nothing'
@

In order to use this 'singlethongs' function, you will need to enable the
following GHC extensions:

@
\{\-\# LANGUAGE DataKinds, GADTs, KindSignatures, TemplateHaskell, TypeFamilies \#\-\}
@
-}

singlethongs :: Name -> Q [Dec]
singlethongs n0 = reify n0 >>= \case
  TyConI (DataD [] n1 [] Nothing cons@(_:_) []) -> do
    nCons <- traverse conName cons
    out0 <- genDataInstSing n1 nCons
    out1 <- genInstanceSingKind n1 nCons
    out2 <- genInstanceTestEquality n1 nCons
    out3 <- mconcat <$> traverse genInstanceSingI nCons
    pure (out0 <> out1 <> out2 <> out3)
  _ -> fail "Only enum types are acceptable"

conName :: Con -> Q Name
conName = \case
  NormalC n [] -> pure n
  _ -> fail "Only enum types are acceptable"

sName :: Name -> Name
sName a = mkName ("S" <> nameBase a)

genDataInstSing :: Name -> [Name] -> Q [Dec]
genDataInstSing nTy nCons = do
  let cons1 = flip fmap nCons $ \nCon ->
        GadtC [sName nCon] [] (AppT (ConT ''Sing) (PromotedT nCon))
  pure [mkSingDataInstD nTy cons1]

genInstanceSingI :: Name -> Q [Dec]
genInstanceSingI nCon = do
  let singD = FunD (mkName "sing") [Clause [] (NormalB (ConE (sName nCon))) []]
  pure [InstanceD Nothing [] (AppT (ConT ''SingI) (PromotedT nCon)) [singD]]

genInstanceSingKind :: Name -> [Name] -> Q [Dec]
genInstanceSingKind nTy nCons = do
  let fromSingD = FunD (mkName "fromSing") $ flip fmap nCons $ \nCon ->
#if MIN_VERSION_template_haskell(2,18,0)
        Clause [ConP (sName nCon) [] []] (NormalB (ConE nCon)) []
#else
        Clause [ConP (sName nCon) []] (NormalB (ConE nCon)) []
#endif
      toSingD = FunD (mkName "toSing") $ flip fmap nCons $ \nCon ->
#if MIN_VERSION_template_haskell(2,18,0)
        Clause [ConP nCon [] []]
#else
        Clause [ConP nCon []]
#endif
               (NormalB (AppE (ConE 'SomeSing) (ConE (sName nCon)))) []
  pure [InstanceD Nothing [] (AppT (ConT ''SingKind) (ConT nTy))
         [mkDemoteD nTy, fromSingD, toSingD] ]

genInstanceTestEquality :: Name -> [Name] -> Q [Dec]
genInstanceTestEquality nTy nCons = do
  let teD = FunD (mkName "testEquality") $ mconcat
       [ flip fmap nCons $ \nCon ->
#if MIN_VERSION_template_haskell(2,18,0)
           let p = ConP (sName nCon) [] []
#else
           let p = ConP (sName nCon) []
#endif
           in Clause [p, p] (NormalB (AppE (ConE 'Just) (ConE 'Refl))) []
       , case nCons of
           [_] -> []
           _ -> [Clause [WildP, WildP] (NormalB (ConE 'Nothing)) []]
       ]
  pure [InstanceD Nothing []
          (AppT (ConT ''TestEquality)
                (SigT (ConT ''Sing)
                      (AppT (AppT ArrowT (ConT nTy)) StarT)))
          [teD ]]


mkDemoteD :: Name -> Dec
mkDemoteD nTy =
#if MIN_VERSION_template_haskell(2,15,0)
  TySynInstD (TySynEqn Nothing (AppT (ConT ''Demote) (ConT nTy)) (ConT nTy))
#else
  TySynInstD ''Demote (TySynEqn [ConT nTy] (ConT nTy))
#endif

mkSingDataInstD :: Name -> [Con] -> Dec
mkSingDataInstD nTy cons =
#if MIN_VERSION_template_haskell(2,15,0)
  DataInstD [] Nothing (AppT (ConT ''Sing) (SigT (VarT (mkName "x")) (ConT nTy)))
            Nothing cons []
#else
  DataInstD [] ''Sing [SigT (VarT (mkName "x")) (ConT nTy)] Nothing cons []
#endif


