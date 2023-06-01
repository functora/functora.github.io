module Dazzle.Ast
  ( Sym (..),
    Exp (..),
    Typ (..),
    Def (..),
    Lit (..),
    Mod (..),
  )
where

import Dazzle.Import

newtype Sym = Sym
  { unSym :: Text
  }
  deriving stock (Eq, Ord, Show, Read, Generic)

data Exp
  = ExpSym Sym
  | ExpPar Exp
  | ExpApp Exp [Exp]
  | ExpLit Lit
  | ExpCase Exp [(Exp, Exp)]
  | ExpTuple [Exp]
  deriving stock (Eq, Ord, Show, Read, Generic)

data Typ
  = TypExp Exp
  | TypFun [Typ] Typ
  | TypDat Sym [Typ]
  deriving stock (Eq, Ord, Show, Read, Generic)

data Def
  = DefFun Sym [(Exp, Typ)] Typ Exp
  | DefDat Sym [Typ] [(Sym, [Typ])]
  deriving stock (Eq, Ord, Show, Read, Generic)

data Lit
  = LitUnit
  | LitBool Bool
  | LitChar Char
  | LitText Text
  | LitIntr Integer
  | LitFrac Text
  deriving stock (Eq, Ord, Show, Read, Generic)

data Mod = Mod
  { modName :: Text,
    modDefs :: [Def]
  }
  deriving stock (Eq, Ord, Show, Read, Generic)
