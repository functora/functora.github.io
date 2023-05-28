{-# OPTIONS_GHC -Wno-deprecations #-}

module Hleam.Transpiler (newMod) where

import qualified Data.Text as T
import GHC.Hs
import GHC.Types.Name.Occurrence
import GHC.Types.Name.Reader
import GHC.Types.SrcLoc
import GHC.Unit.Module
import GHC.Utils.Outputable hiding ((<>))
import Hleam.Ast
import Hleam.Import

newMod :: HsModule -> Mod
newMod x =
  Mod
    { modName =
        maybe "unnamed" (T.pack . moduleNameString . unLoc) $ hsmodName x,
      modDefs =
        hsmodDecls x >>= newDef . unLoc
    }

newDef :: HsDecl GhcPs -> [Def]
newDef = \case
  ValD _ (FunBind _ _ (MG _ clauses _) _) ->
    case unLoc <$> unLoc clauses of
      [Match _ fun args (GRHSs _ expr _)] ->
        newFun fun (unLoc <$> args) $ unLoc <$> expr
      e ->
        failure "newDef" e
  SigD {} ->
    mempty
  e ->
    failure "newDef" e

newFun ::
  HsMatchContext GhcPs ->
  [Pat GhcPs] ->
  [GRHS GhcPs (GenLocated SrcSpanAnnA (HsExpr GhcPs))] ->
  [Def]
newFun (FunRhs fun _ _) args [GRHS _ _ expr] =
  singleton
    . DefFun
      (newVar fun)
      (newArg <$> args)
      (TypVar $ Var "Output")
    . newExp
    $ unLoc expr
newFun e _ _ =
  failure "newFun" e

newArg :: Pat GhcPs -> (Var, Typ)
newArg = \case
  VarPat _ x -> (newVar x, TypVar $ Var "Input")
  e -> failure "newArg" e

newExp :: HsExpr GhcPs -> Exp
newExp = \case
  HsVar _ x -> ExpVar $ newVar x
  HsApp _ lhs rhs -> ExpApp (newExp $ unLoc lhs) [newExp $ unLoc rhs]
  e -> failure "newExp" e

newVar :: GenLocated a RdrName -> Var
newVar =
  Var
    . T.pack
    . occNameString
    . rdrNameOcc
    . unLoc

failure :: Outputable a => Text -> a -> any
failure tag =
  error
    . (mappend $ "Unsupported " <> tag <> " ")
    . T.pack
    . showPprUnsafe
    . ppr
