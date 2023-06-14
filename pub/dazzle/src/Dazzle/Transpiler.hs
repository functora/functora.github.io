module Dazzle.Transpiler (newMod) where

import qualified Data.Char as C
import qualified Data.Map as Map
import qualified Data.Text as T
import Dazzle.Ast
import Dazzle.Import
import GHC.Data.FastString
import GHC.Hs
import GHC.Types.Name.Occurrence
import GHC.Types.Name.Reader
import GHC.Types.SrcLoc
import GHC.Unit.Module
import GHC.Utils.Outputable hiding ((<>))
import qualified Text.Casing as T
import qualified Universum.Unsafe as Unsafe

newMod :: HsModule -> Mod
newMod x =
  Mod
    { modName =
        maybe
          "unnamed"
          (T.pack . moduleNameString . unLoc)
          $ hsmodName x,
      modDefs =
        dcs >>= newDef (foldl' newSigMap mempty dcs)
    }
  where
    dcs = unLoc <$> hsmodDecls x

newSigMap :: Map Sym [Exp] -> HsDecl GhcPs -> Map Sym [Exp]
newSigMap acc = \case
  SigD _ (TypeSig _ [fun] (HsWC _ args0)) ->
    Map.insert
      (newSym fun)
      ( let HsSig _ _ args1 = unLoc args0
         in unArrow . newSig $ unLoc args1
      )
      acc
  _ ->
    acc

newSig :: HsType GhcPs -> Exp
newSig = \case
  HsTyVar _ _ x ->
    ExpSym $ newSym x
  HsFunTy _ _ lhs rhs ->
    ExpArrow (newSig $ unLoc lhs) . newSig $ unLoc rhs
  HsAppTy _ fun args ->
    ExpApp (newSig $ unLoc fun) . singleton . newSig $ unLoc args
  HsTupleTy _ _ xs ->
    ExpTuple $ newSig . unLoc <$> xs
  e ->
    failure "newSig" e

--
-- TODO : not ideal, need to fix this and rederer!!!
--
unArrow :: Exp -> [Exp]
unArrow = \case
  ExpArrow lhs rhs -> unArrow lhs <> unArrow rhs
  x -> singleton x

newDef :: Map Sym [Exp] -> HsDecl GhcPs -> [Def]
newDef sigs = \case
  ValD _ (FunBind _ _ (MG _ clauses _) _) ->
    case unLoc <$> unLoc clauses of
      [Match _ fun args (GRHSs _ expr _)] ->
        newDefFun sigs fun (unLoc <$> args) $ unLoc <$> expr
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
                UserTyVar _ _ x -> ExpSym $ newSym x
                e -> failure "newDef-2" e
            )
            $ unLoc <$> args
        )
        ( fmap
            ( \case
                ConDeclH98 _ con _ _ _ conArgs _ ->
                  (newSym con, newCon conArgs)
                e ->
                  failure "newDef-3" e
            )
            $ unLoc <$> cons
        )
  e ->
    failure "newDef" e

newCon :: HsConDeclH98Details GhcPs -> [(Maybe Sym, Exp)]
newCon = \case
  RecCon rec ->
    fmap
      ( \case
          ConDeclField _ [field] typ _ ->
            ( Just
                . newSym
                . foLabel
                $ unLoc field,
              typFun
                . newSig
                $ unLoc typ
            )
          e ->
            failure "newCon" e
      )
      $ unLoc <$> unLoc rec
  PrefixCon _ args ->
    fmap
      ( \case
          HsScaled _ typ ->
            ( Nothing,
              typFun
                . newSig
                $ unLoc typ
            )
      )
      args
  _ ->
    error ("TODO newCon" :: Text)

typFun :: Exp -> Exp
typFun expr =
  case expr of
    ExpArrow {} ->
      case reverse $ unArrow expr of
        ret : lst : args ->
          ExpFun FunTyp (reverse (lst : args)) ret
        _ ->
          error $ "typFun : " <> show expr
    _ ->
      expr

newDefFun ::
  Map Sym [Exp] ->
  HsMatchContext GhcPs ->
  [Pat GhcPs] ->
  [GRHS GhcPs (GenLocated SrcSpanAnnA (HsExpr GhcPs))] ->
  [Def]
newDefFun sigs (FunRhs fun _ _) args [GRHS _ _ expr] =
  let name = newSym fun
   in maybe
        ( error $
            "Could not find a signature for a " <> show name
        )
        ( \typs ->
            singleton
              . DefFun
                name
                ( fmap
                    ( \(idx :: Int, pat) ->
                        ( newExpPat pat,
                          Unsafe.at idx typs
                        )
                    )
                    $ zip [0 ..] args
                )
                ( Unsafe.at (length args) typs
                )
              . newExp
              $ unLoc expr
        )
        $ Map.lookup name sigs
newDefFun _ e _ _ =
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
  HsLam _ (MG _ clauses _) ->
    case unLoc <$> unLoc clauses of
      [Match _ LambdaExpr args (GRHSs _ rhs _)] ->
        case unLoc <$> rhs of
          [GRHS _ _ expr] ->
            ExpFun FunLam (newExpPat . unLoc <$> args) . newExp $ unLoc expr
          _ ->
            failure "newExp-7" clauses
      e ->
        failure "newExp-6" e
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
newSym raw =
  Sym . T.pack $ case safeHead str of
    Nothing -> str
    Just x ->
      if C.isLower x
        then T.quietSnake str
        else T.pascal str
  where
    str =
      occNameString
        . rdrNameOcc
        $ unLoc raw

failure :: Outputable a => Text -> a -> any
failure tag =
  error
    . mappend ("Unsupported " <> tag <> " ")
    . T.pack
    . showPprUnsafe
    . ppr
