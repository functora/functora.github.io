{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Functora.Miso.Prelude
  ( module X,
    inspect,
    consoleLog,
  )
where

import Functora.Cfg as X
import Functora.Miso.Orphan as X ()
import Functora.Prelude as X hiding
  ( Field (..),
    String,
    Text,
    cons,
    field,
    inspect,
  )
import qualified Functora.Prelude as Prelude
import Miso as X hiding
  ( Key,
    Text,
    URI,
    at,
    consoleLog,
    for_,
    view,
  )
import qualified Miso
import Miso.String as X
  ( FromMisoString (..),
    MisoString,
    ToMisoString (..),
    fromMisoString,
  )
import Type.Reflection

instance (ToMisoString a) => ToMisoString (Tagged "UTF-8" a) where
  toMisoString = toMisoString . unTagged

instance (FromMisoString a) => FromMisoString (Tagged "UTF-8" a) where
  fromMisoStringEither = Right . Tagged @"UTF-8" . fromMisoString

inspect :: (Show a, Data a) => a -> MisoString
inspect x =
  case typeOf x `eqTypeRep` typeRep @MisoString of
    Just HRefl -> x
    Nothing -> toMisoString $ Prelude.inspect @Prelude.Text x

consoleLog :: (Show a, Data a) => a -> JSM ()
consoleLog = Miso.consoleLog . inspect
