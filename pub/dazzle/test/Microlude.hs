module Microlude
  ( Proxy (..),
    Tagged (..),
    Lens (..),
    fst,
  )
where

data Proxy tags = Proxy

newtype Tagged tags rep = Tagged
  { unTagged :: rep
  }

data Lens s a = Lens
  { view :: s -> a,
    set :: s -> a -> s
  }

fst :: Lens (a, b) a
fst = Lens (\(x, _) -> x) (\(_, y) x -> (x, y))
