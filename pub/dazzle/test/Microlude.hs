{-# LANGUAGE OverloadedRecordDot #-}

module Microlude
  ( Tagged (..),
    Foo (..),
    Bar (..),
    unFooBar,
  )
where

import Prelude

newtype Tagged tags rep = Tagged
  { unTagged :: rep
  }

newtype Foo = Foo {unFoo :: Int}

newtype Bar = Bar {unBar :: Foo}

unFooBar :: Bar -> Int
unFooBar x = x.unBar.unFoo

--
-- TODO : after fix https://github.com/gleam-lang/gleam/issues/2179
--
-- data Proxy tags = Proxy
--
-- data Lens s a = Lens
--   { view :: s -> a,
--     set :: s -> a -> s
--   }
--
-- fst :: Lens (a, b) a
-- fst = Lens (\(x, _) -> x) (\(_, y) x -> (x, y))
