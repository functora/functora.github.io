module Dazzle.Renderer
  ( Tab (..),
    renModFmt,
    fmt,
    renMod,
  )
where

import Dazzle.Ast
import Dazzle.Import

newtype Tab = Tab
  { unTab :: Natural
  }
  deriving newtype (Eq, Ord, Show, Read)

addTab :: Tab -> Tab
addTab tab =
  Tab $ 2 + unTab tab

renTab :: Tab -> Text
renTab tab =
  mconcat $
    replicate (unsafeFrom @Natural @Int $ unTab tab) " "

renModFmt :: Mod -> IO (Either Text Text)
renModFmt =
  fmt . renMod

fmt :: Text -> IO (Either Text Text)
fmt src =
  catch
    ( fmap
        ( \case
            (ExitSuccess, txt, _) ->
              Right $
                from @String @Text txt
            (ExitFailure code, _, txt) ->
              Left $
                "EXIT CODE "
                  <> show code
                  <> "\n"
                  <> from @String @Text txt
        )
        . readCreateProcessWithExitCode (proc "gleam" ["format", "--stdin"])
        $ from @Text @String src
    )
    $ pure
      . Left
      . (mappend "EXCEPTION ")
      . show @Text @IOException

renMod :: Mod -> Text
renMod =
  intercalate "\n\n"
    . fmap (renDef (Tab 0))
    . modDefs

renDef :: Tab -> Def -> Text
renDef tab (DefFun name args rtrn expr) =
  if null args
    then
      "pub const "
        <> renSym name
        <> ": "
        <> renTyp rtrn
        <> " = "
        <> renExp tab expr
    else
      "pub fn "
        <> renSym name
        <> "("
        <> intercalate
          ", "
          ( fmap
              ( \(lhs, rhs) ->
                  renExp tab (unExpPar lhs)
                    <> ": "
                    <> renTyp rhs
              )
              args
          )
        <> ") -> "
        <> renTyp rtrn
        <> " {\n"
        <> renTab (addTab tab)
        <> renExp (addTab tab) expr
        <> "}"
renDef _ (DefDat name args cons) =
  --
  -- TODO : record accessors
  --
  "pub type "
    <> unSym name
    <> ( if null args
           then mempty
           else "(" <> intercalate ", " (renTyp <$> args) <> ")"
       )
    <> " {\n  "
    <> intercalate
      "\n  "
      ( fmap
          ( \(con, fields) ->
              renSym con
                <> ( if null fields
                       then mempty
                       else
                         "("
                           <> intercalate
                             ", "
                             ( fmap
                                 ( \(mtag, typ) ->
                                     ( maybe
                                         mempty
                                         (\s -> renSym s <> ": ")
                                         mtag
                                     )
                                       <> renTyp typ
                                 )
                                 fields
                             )
                           <> ")"
                   )
          )
          cons
      )
    <> "\n}"

renSym :: Sym -> Text
renSym = unSym

renTyp :: Typ -> Text
renTyp = \case
  TypExp x ->
    renExp (Tab 0) x
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

renExp :: Tab -> Exp -> Text
renExp tab = \case
  ExpSym x ->
    renSym x
  ExpPar x ->
    "{" <> renExp tab x <> "}"
  ExpApp x xs ->
    renExp tab x <> "(" <> intercalate ", " (renExp tab <$> xs) <> ")"
  ExpLit x ->
    renLit x
  ExpCase x xs ->
    "case "
      <> renExp tab x
      <> " {\n"
      <> renTab (addTab tab)
      <> intercalate
        ("\n" <> renTab (addTab tab))
        ( fmap
            ( \(lhs, rhs) ->
                renExp tab (unExpPar lhs)
                  <> " -> "
                  <> renExp tab rhs
            )
            xs
        )
      <> "\n"
      <> renTab tab
      <> "}\n"
  ExpTuple xs ->
    "#("
      <> intercalate ", " (fmap (renExp tab) xs)
      <> ")"
  e@ExpArrow {} ->
    error $ "ExpArrow renderer does not exist " <> show e

renLit :: Lit -> Text
renLit = \case
  LitUnit -> "Nil"
  LitBool x -> show x
  LitChar x -> show $ fromEnum x
  LitText x -> from @String @Text $ ushow x
  LitIntr x -> show x
  LitFrac x -> x

unExpPar :: Exp -> Exp
unExpPar = \case
  ExpPar x -> unExpPar x
  x -> x
