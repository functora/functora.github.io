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
  if null args
    then
      "pub const "
        <> renSym name
        <> ": "
        <> renTyp rtrn
        <> " = "
        <> renExp expr
    else
      "pub fn "
        <> renSym name
        <> "("
        <> intercalate
          ", "
          (fmap (\(lhs, rhs) -> renExp lhs <> ": " <> renTyp rhs) args)
        <> ") -> "
        <> renTyp rtrn
        <> " {"
        <> renExp expr
        <> "}"
renDef (DefDat name _ _) =
  "pub type "
    <> unSym name
    <> " {\n"
    <> "  TODO\n}"

renSym :: Sym -> Text
renSym = unSym

renTyp :: Typ -> Text
renTyp = \case
  TypExp x ->
    renExp x
  TypFun xs x ->
    "fn("
      <> intercalate ", " (renTyp <$> xs)
      <> ") -> "
      <> renTyp x
  TypDat x xs ->
    renSym x
      <> "("
      <> intercalate ", " (renTyp <$> xs)
      <> ")"

renExp :: Exp -> Text
renExp = \case
  ExpSym x ->
    renSym x
  ExpPar x ->
    "(" <> renExp x <> ")"
  ExpApp x xs ->
    renExp x <> "(" <> intercalate ", " (renExp <$> xs) <> ")"
  ExpLit x ->
    renLit x
  ExpCase x xs ->
    "case "
      <> renExp x
      <> " {\n"
      <> intercalate
        "\n"
        ( fmap
            (\(lhs, rhs) -> "  " <> renExp lhs <> " -> " <> renExp rhs)
            xs
        )
      <> "\n}"
  ExpTuple xs ->
    "#("
      <> intercalate ", " (fmap renExp xs)
      <> ")"

renLit :: Lit -> Text
renLit = \case
  LitUnit -> "Nil"
  LitBool x -> show x
  LitChar x -> show x
  LitText x -> show x
  LitIntr x -> show x
  LitFrac x -> x

-- pub fn fmap_list(lhs: fn(a) -> b, rhs: List(a)) -> List(b) {
--   fmap(lhs, rhs, list_functor_instance)
-- }
