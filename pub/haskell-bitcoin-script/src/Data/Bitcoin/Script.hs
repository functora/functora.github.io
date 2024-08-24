{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Bitcoin.Script
  ( decode,
    encode,
    ScriptOp (..),
    Script (..),
    PushDataType (..),
  )
where

import qualified Data.Binary as B
import Data.Bitcoin.Script.Types
import qualified Data.ByteString.Base16.Lazy as BS16L
import qualified Data.ByteString.Lazy as BSL

-- | Decodes a hex representation of a script into a 'Script' object.
decode :: BSL.ByteString -> Script
decode =
  B.decode . handler . BS16L.decode
  where
#if MIN_VERSION_base16_bytestring(1,0,0)
      handler :: Either String BSL.ByteString -> BSL.ByteString
      handler =
        either error id
#else
      handler :: (BSL.ByteString, BSL.ByteString) -> BSL.ByteString
      handler = \case
        (success, "") -> success
        failure -> error $ "Non-Hex bytestring " <> show failure
#endif

-- | Encodes a 'Script' object into a hex representation
encode :: Script -> BSL.ByteString
encode =
  BS16L.encode . B.encode
