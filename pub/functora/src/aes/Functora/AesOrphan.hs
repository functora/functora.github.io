{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Functora.AesOrphan () where

import Data.Bits
import Data.LargeWord

instance FiniteBits Word96 where finiteBitSize = bitSize

instance FiniteBits Word128 where finiteBitSize = bitSize

instance FiniteBits Word160 where finiteBitSize = bitSize

instance FiniteBits Word192 where finiteBitSize = bitSize

instance FiniteBits Word224 where finiteBitSize = bitSize

instance FiniteBits Word256 where finiteBitSize = bitSize
