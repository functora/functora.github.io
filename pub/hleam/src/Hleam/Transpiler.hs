module Hleam.Transpiler (newMod) where

import qualified Data.Text as T
import GHC.Data.FastString
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
        newDefFun fun (unLoc <$> args) $ unLoc <$> expr
      e ->
        failure "newDef" e
  SigD {} ->
    mempty
  TyClD _ (DataDecl _ sym (HsQTvs _ args) _ (HsDataDefn _ _ _ _ _ cons _)) ->
    singleton $
      DefDat
        (newSym sym)
        ( fmap
            ( \case
                UserTyVar _ _ x -> TypExp . ExpSym $ newSym x
                e -> failure "newDef-2" e
            )
            $ unLoc <$> args
        )
        ( fmap
            ( \case
                ConDeclH98 _ con _ _ _ _ _ -> (newSym con, mempty)
                e -> failure "newDef-3" e
            )
            $ unLoc <$> cons
        )
  e ->
    failure "newDef" e

newDefFun ::
  HsMatchContext GhcPs ->
  [Pat GhcPs] ->
  [GRHS GhcPs (GenLocated SrcSpanAnnA (HsExpr GhcPs))] ->
  [Def]
newDefFun (FunRhs fun _ _) args [GRHS _ _ expr] =
  singleton
    . DefFun
      (newSym fun)
      (((,TypExp . ExpSym $ Sym "Input") . newExpPat) <$> args)
      (TypExp . ExpSym $ Sym "Output")
    . newExp
    $ unLoc expr
newDefFun e _ _ =
  failure "newDefFun" e

newExpPat :: Pat GhcPs -> Exp
newExpPat = \case
  VarPat _ x ->
    ExpSym $ newSym x
  ParPat _ _ x _ ->
    ExpPar . newExpPat $ unLoc x
  ConPat _ x _ ->
    ExpSym $ newSym x
  TuplePat _ xs _ ->
    ExpTuple $ newExpPat . unLoc <$> xs
  LitPat _ x ->
    ExpLit $ newLit x
  WildPat _ ->
    ExpSym $ Sym "_"
  e ->
    failure "newExpPat" e

newExp :: HsExpr GhcPs -> Exp
newExp = \case
  HsVar _ x ->
    ExpSym $ newSym x
  HsPar _ _ x _ ->
    ExpPar . newExp $ unLoc x
  HsApp _ fun0 args0 ->
    --
    -- NOTE : simple way to uncurry
    --
    let args1 = singleton . newExp $ unLoc args0
     in case newExp $ unLoc fun0 of
          ExpApp fun args -> ExpApp fun $ args <> args1
          fun -> ExpApp fun args1
  e@(ExplicitTuple _ xs _) ->
    ExpTuple $
      fmap
        ( \case
            Present _ x -> newExp $ unLoc x
            _ -> failure "HsExplicitTuple" e
        )
        xs
  HsCase _ exp0 (MG _ cls0 _) ->
    ExpCase (newExp $ unLoc exp0)
      . fmap
        ( \case
            Match _ CaseAlt [lhs] (GRHSs _ rhs _) ->
              case unLoc <$> rhs of
                [GRHS _ _ expr] ->
                  (newExpPat $ unLoc lhs, newExp $ unLoc expr)
                _ ->
                  failure "newExp-5" exp0
            Match _ e _ (GRHSs _ _ _) ->
              failure "newExp-4" e
        )
      $ unLoc <$> unLoc cls0
  HsLit _ lit ->
    ExpLit $ newLit lit
  e ->
    failure "newExp-3" e

newLit :: HsLit GhcPs -> Lit
newLit = \case
  HsChar _ x -> LitChar x
  HsString _ x -> LitText . T.pack $ unpackFS x
  e -> failure "newLit" e

-- -- | HsCharPrim (XHsCharPrim x) {- SourceText -} Char
-- -- | HsStringPrim (XHsStringPrim x) {- SourceText -} !ByteString
-- -- | HsInt (XHsInt x)  IntegralLit
-- -- | HsIntPrim (XHsIntPrim x) {- SourceText -} Integer
-- -- | HsWordPrim (XHsWordPrim x) {- SourceText -} Integer
-- -- | HsInt64Prim (XHsInt64Prim x) {- SourceText -} Integer
-- -- | HsWord64Prim (XHsWord64Prim x) {- SourceText -} Integer
-- -- | HsInteger (XHsInteger x) {- SourceText -} Integer Type
-- -- | HsRat (XHsRat x)  FractionalLit Type
-- -- | HsFloatPrim (XHsFloatPrim x)   FractionalLit
-- -- | HsDoublePrim (XHsDoublePrim x) FractionalLit

newSym :: GenLocated a RdrName -> Sym
newSym =
  Sym
    . T.pack
    . occNameString
    . rdrNameOcc
    . unLoc

failure :: Outputable a => Text -> a -> any
failure tag =
  error
    . mappend ("Unsupported " <> tag <> " ")
    . T.pack
    . showPprUnsafe
    . ppr
