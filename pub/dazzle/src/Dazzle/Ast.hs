module Dazzle.Ast
  ( Sym (..),
    Fun (..),
    Exp (..),
    Def (..),
    Lit (..),
    Mod (..),
  )
where

import Dazzle.Import

newtype Sym = Sym
  { unSym :: Text
  }
  deriving stock (Eq, Ord, Show, Read, Generic, Data)

data Fun
  = FunLam
  | FunTyp
  deriving stock (Eq, Ord, Show, Read, Generic, Data)

data Exp
  = ExpSym Sym
  | ExpPar Exp
  | ExpApp Exp [Exp]
  | ExpFun Fun [Exp] Exp
  | ExpInf Exp Exp Exp
  | ExpLit Lit
  | ExpCase Exp [(Exp, Exp)]
  | ExpTuple [Exp]
  | --
    -- TODO : remove somehow!!!
    --
    ExpArrow Exp Exp
  deriving stock (Eq, Ord, Show, Read, Generic, Data)

data Def
  = DefFun
      { _defFunName :: Sym,
        _defFunArgs :: [(Exp, Exp)],
        _defFunRtrn :: Exp,
        _defFunBody :: Exp
      }
  | DefDat
      { _defDatName :: Sym,
        _defDatArgs :: [Exp],
        _defDatCons :: [(Sym, [(Maybe Sym, Exp)])]
      }
  deriving stock (Eq, Ord, Show, Read, Generic, Data)

data Lit
  = LitUnit
  | LitBool Bool
  | LitChar Char
  | LitText Text
  | LitIntr Integer
  | LitFrac Text
  deriving stock (Eq, Ord, Show, Read, Generic, Data)

data Mod = Mod
  { modName :: Text,
    modDefs :: [Def]
  }
  deriving stock (Eq, Ord, Show, Read, Generic, Data)
