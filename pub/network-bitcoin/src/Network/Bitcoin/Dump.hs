{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
-- | An interface to bitcoind's available private key calls. The implementation
--   of these functions can be found at <https://github.com/bitcoin/bitcoin/blob/master/src/rpcdump.cpp>.
--
--   If any APIs are missing, patches are always welcome. If you look at the
--   source of this module, you'll see that the interface code is trivial.
module Network.Bitcoin.Dump ( PrivateKey
                            , importPrivateKey
                            , dumpPrivateKey
                            ) where

import           Network.Bitcoin.Internal

-- | A textual representation of a bitcoin private key.
type PrivateKey = Text

-- | Adds a private key (as returned by dumpprivkey) to your wallet.
importPrivateKey :: Client
                 -> PrivateKey
                 -> Maybe Account
                 -- ^ An optional label for the key.
                 -> IO ()
importPrivateKey client pk Nothing =
    unNil <$> callApi client "importprivkey" [ tj pk ]
importPrivateKey client pk (Just label) =
    unNil <$> callApi client "importprivkey" [ tj pk, tj label ]

-- | Reveals the private key corresponding to the given address.
dumpPrivateKey :: Client
               -> Address
               -> IO PrivateKey
dumpPrivateKey client addr = callApi client "dumpprivkey" [ tj addr ]
