{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# OPTIONS_GHC -Wall #-}
-- | Contains the common types used through bitcoin RPC calls, that aren't
--   specific to a single submodule.
module Network.Bitcoin.Types ( Client
                             , BitcoinException(..)
                             , HexString
                             , TransactionID(..)
                             , Satoshi(..)
                             , BTC
                             , Account
                             , Address
                             ) where

import           Control.Exception
import           GHC.Generics (Generic)
import qualified Data.ByteString.Lazy as BL
import           Data.Fixed
import           Data.Text            (Text)
import           Control.Monad
import           Data.Typeable
import Data.Aeson

-- | 'Client' describes authentication credentials and host info for
-- making API requests to the Bitcoin daemon.
type Client = BL.ByteString -> IO BL.ByteString

-- | A 'BitcoinException' is thrown when 'callApi encounters an
--   error.  The API error code is represented as an @Int@, the message as
--   a @String@.
--
--   It may also be thrown when the value returned by the bitcoin API wasn't
--   what we expected.
--
--   WARNING: Any of the functions in this module's public API may throw this
--            exception. You should plan on handling it.
data BitcoinException = BitcoinApiError Int Text
                      -- ^ A 'BitcoinApiError' has an error code error
                      --   message, as returned by bitcoind's JSON-RPC
                      --   response.
                      | BitcoinResultTypeError BL.ByteString
                      -- ^ The raw JSON returned, if we can't figure out what
                      --   actually went wrong.
    deriving ( Show, Read, Ord, Eq, Typeable, Generic )

instance Exception BitcoinException

-- | A string returned by the bitcoind API, representing data as hex.
--
--   What that data represents depends on the API call, but should be
--   dcumented accordingly.
type HexString = Text

-- | A hexadecimal string representation of a 256-bit unsigned integer.
--
--   This integer is a unique transaction identifier.
newtype TransactionID = TransactionID { unTransactionID :: Text }
    deriving (Ord, Eq, Show, Read)

instance FromJSON TransactionID where
    parseJSON (String i) = pure $ TransactionID i
    parseJSON _ = mzero

instance ToJSON TransactionID where
    toJSON (TransactionID txid) = String txid

-- | A satoshi is the smallest subdivision of bitcoins. For the resolution,
--   use 'resolution' from 'Data.Fixed'.
data Satoshi = Satoshi

instance HasResolution Satoshi where
    resolution = const $ 10^(8::Integer)
    {-# INLINE resolution #-}

-- | The type of bitcoin money, represented with a fixed-point number.
type BTC = Fixed Satoshi

-- | An address for sending or receiving money.
type Address = HexString

-- | An account on the wallet is just a label to easily specify private keys.
--
--   The default account is an empty string.
type Account = Text

