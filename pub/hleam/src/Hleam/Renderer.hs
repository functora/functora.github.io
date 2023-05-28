{-# OPTIONS_GHC -Wno-deprecations #-}

module Hleam.Renderer (renMod) where

import Hleam.Ast
import Hleam.Import

renMod :: Mod -> Text
renMod =
  intercalate "\n\n"
    . fmap renDef
    . modDefs

renDef :: Def -> Text
renDef (DefFun name args rtrn expr) =
  "pub fn "
    <> renVar name
    <> "("
    <> intercalate
      ", "
      ((\(var, typ) -> renVar var <> ": " <> renTyp typ) <$> args)
    <> ") -> "
    <> renTyp rtrn
    <> " {"
    <> renExp expr
    <> "}"

renVar :: Var -> Text
renVar = unVar

renTyp :: Typ -> Text
renTyp = \case
  TypVar x ->
    renVar x
  TypCon x xs ->
    unCon x
      <> "("
      <> intercalate ", " (renTyp <$> xs)
      <> ")"
  TypFun xs x ->
    "fn("
      <> intercalate ", " (renTyp <$> xs)
      <> ") -> "
      <> renTyp x

renExp :: Exp -> Text
renExp = \case
  ExpApp x xs ->
    renExp x
      <> "("
      <> intercalate ", " (renExp <$> xs)
      <> ")"
  ExpVar x ->
    unVar x
  ExpCon x ->
    unCon x

-- pub fn fmap_list(lhs: fn(a) -> b, rhs: List(a)) -> List(b) {
--   fmap(lhs, rhs, list_functor_instance)
-- }
