module Microlude
  ( Proxy (..),
    Tagged (..),
  )
where

data Proxy tags = Proxy

newtype Tagged tags rep = Tagged
  { unTagged :: rep
  }
