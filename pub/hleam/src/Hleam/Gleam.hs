module Hleam.Gleam
  ( Mod (..),
    Def (..),
    Typ (..),
    Exp (..),
    Lit (..),
    Var (..),
    Con (..),
  )
where

import Hleam.Import

data Mod = Mod
  { modName :: Text,
    modDefs :: [Def]
  }
  deriving stock (Eq, Ord, Show, Read, Generic)

data Def
  = DefFun Var [(Var, Typ)] Typ Exp
  deriving stock (Eq, Ord, Show, Read, Generic)

data Typ
  = TypVar Var
  | TypCon Con [Typ]
  | TypFun [Typ] Typ
  deriving stock (Eq, Ord, Show, Read, Generic)

data Exp
  = ExpApp Exp [Exp]
  | ExpInf Exp Var Exp
  | ExpLet Var Exp
  | ExpDo [Exp]
  | ExpIf Exp Exp Exp
  | ExpLit Lit
  | ExpVar Var
  | ExpCon Con
  deriving stock (Eq, Ord, Show, Read, Generic)

data Lit
  = LitUnit
  | LitBool Bool
  | LitChar Char
  | LitString Text
  | LitInt Integer
  | LitFloat Text
  deriving stock (Eq, Ord, Show, Read, Generic)

newtype Var = Var
  { unVar :: Text
  }
  deriving newtype (Eq, Ord, Show, Read)
  deriving stock (Generic)

newtype Con = Con
  { unCon :: Text
  }
  deriving newtype (Eq, Ord, Show, Read)
  deriving stock (Generic)
