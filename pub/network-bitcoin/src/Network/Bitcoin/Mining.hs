{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
-- | An interface to bitcoind's available mining RPC calls. The implementation
--   of these functions can be found at <https://github.com/bitcoin/bitcoin/blob/master/src/rpcmining.cpp>.
--
--   If any APIs are missing, patches are always welcome. If you look at the
--   source of this module, you'll see that the interface code is trivial.
--
--   Note that it is highly discouraged to use bitcoind for actual bitcoin
--   mining. It uses the CPU for mining, which is much, much less power
--   efficient than GPU mining. If you're paying for power, you will not come
--   out ahead.
--
--   Instead, consider using a GPU miner listed at <https://en.bitcoin.it/wiki/Software#Mining_apps>.
module Network.Bitcoin.Mining ( Client
                              , getClient
                              , getGenerate
                              , setGenerate
                              , generate
                              , generateToAddress
                              , getHashesPerSec
                              , MiningInfo(..)
                              , getMiningInfo
                              , HashData(..)
                              , getWork
                              , solveBlock
                              , Transaction(..)
                              , CoinBaseAux(..)
                              , BlockTemplate(..)
                              , getBlockTemplate
                              , submitBlock
                              ) where

import           Control.Exception        (catch, throw)
import           Control.Monad
import           Data.Aeson               as A
import           Network.Bitcoin.Internal
import           Network.Bitcoin.Wallet   (getNewAddress)
import           Numeric.Natural          (Natural)
import           Control.Concurrent       (threadDelay)

-- | Returns whether or not bitcoind is generating bitcoins.
getGenerate :: Client -- ^ bitcoind RPC client
            -> IO Bool
getGenerate client = callApi client "getgenerate" []

-- | Controls whether or not bitcoind is generating bitcoins.
--   If bitcoind runs in regtest mode the number of generated hashes is returned.
--   See https://bitcoin.org/en/developer-reference#setgenerate for more details.
setGenerate :: Client -- ^ bitcoind RPC client
            -> Bool -- ^ Turn it on, or turn it off?
            -> Maybe Int -- ^ Generation is limited to this number of
                         --   processors. Set it to Nothing to keep the value
                         --   at what it was before, Just -1 to use all
                         --   available cores, and any other value to limit it.
                         --   If bitcoind runs in regtest mode instead of the number of processors,
                         --   this specifies the number of hashes to generate.
            -> IO (Maybe [HexString])
setGenerate client onOff Nothing =
    unArr <$> callApi client "setgenerate" [ tj onOff ]
setGenerate client onOff (Just limit) =
    unArr <$> callApi client "setgenerate" [ tj onOff, tj limit ]

-- | The generate RPC nearly instantly generates blocks.
--   See https://bitcoin.org/en/developer-reference#generate for more details.
generate :: Client
         -> Int -- ^ The number of blocks to generate.
                --   The RPC call will not return until all
                --   blocks have been generated or the maxium
                --   number of iterations has been reached
         -> Maybe Int -- ^ The maximum number of iterations that
                      --   are tried to create the requested number
                      --   of blocks. Default is 1000000
         -> IO [HexString] -- ^ An array containing the block header
                           --   hashes of the generated blocks
                           --   (may be empty if used with generate 0)
generate client blocks maxTries =
    callApi client "generate" args `catch` onFail
  where
    args = tj blocks : maybe [] (pure . tj) maxTries
    -- https://github.com/bitcoin/bitcoin/blob/48174c0f287b19931ca110670610bd03a03eb914/src/rpc/protocol.h#L39
    onFail (BitcoinApiError (-1) _) =
      getNewAddress client Nothing
        >>= flip (generateToAddress client blocks) maxTries
    onFail e =
      throw e

-- | The generatetoaddress RPC mines blocks immediately to a specified address.
--   See https://bitcoin.org/en/developer-reference#generatetoaddress for more details.
generateToAddress :: Client
                  -> Int -- ^ The number of blocks to generate.
                         --   The RPC call will not return until all
                         --   blocks have been generated or the maxium
                         --   number of iterations has been reached
                  -> Address -- ^ The address to send the newly generated Bitcoin to
                  -> Maybe Int -- ^ The maximum number of iterations that
                               --   are tried to create the requested number
                               --   of blocks. Default is 1000000
                  -> IO [HexString]
generateToAddress client blocks address maxTries =
    action `catch` onFail 10
  where
    args = tj blocks : tj address : maybe [] (pure . tj) maxTries
    action = callApi client "generatetoaddress" args
    -- https://github.com/bitcoin/bitcoin/issues/24730
    onFail :: Natural -> BitcoinException -> IO [HexString]
    onFail 0 e =
      throw e
    onFail attempt (BitcoinApiError (-32603) "ProcessNewBlock, block not accepted") = do
      -- sleep 0.1 sec and retry
      threadDelay 100000
      action `catch` onFail (attempt - 1)
    onFail _ e =
      throw e

-- | Returns a recent hashes per second performance measurement while
--   generating.
getHashesPerSec :: Client -> IO Integer
getHashesPerSec client = callApi client "gethashespersec" []

-- | Information related to the current bitcoind mining operation.
--
--   If a field is undocumented here, it's because I don't know what it means.
--   If you DO know what it means, I'd love it if you would submit a patch to
--   help complete this documentation.
data MiningInfo =
    MiningInfo {
               -- | The number of blocks in our block-chain.
                 nBlocks                  :: Integer
               -- | The size of the current block we're mining.
               , currentBlockSize         :: Integer
               , currentBlockTransaction  :: Integer
               -- | How difficult mining currently is.
               , difficulty               :: Double
               -- | Any mining errors that may have come up.
               , miningErrors             :: Text
               -- | Are we currently generating bitcoins?
               , isGenerating             :: Bool
               -- | How many processors have we limited bitcoin mining to?
               , generationProcessorLimit :: Integer
               -- | How fast is the mining going?
               , hashesPerSecond          :: Integer
               , pooledTransactions       :: Integer
               -- | Are we on the bitcoin test network (as opposed to the real
               --   thing)?
               , miningOnTestNetwork      :: Bool
               }
    deriving ( Show, Read, Ord, Eq )

instance FromJSON MiningInfo where
    parseJSON (Object o) = MiningInfo <$> o .: "blocks"
                                      <*> o .: "currentblocksize"
                                      <*> o .: "currentblocktx"
                                      <*> o .: "difficulty"
                                      <*> o .: "errors"
                                      <*> o .: "generate"
                                      <*> o .: "genproclimit"
                                      <*> o .: "hashespersec"
                                      <*> o .: "pooledtx"
                                      <*> o .: "testnet"
    parseJSON _ = mzero

-- | Returns an object containing mining-related information.
getMiningInfo :: Client -> IO MiningInfo
getMiningInfo client = callApi client "getmininginfo" []

-- | The hash data returned from 'getWork'.
data HashData =
    HashData { blockData :: HexString
             -- | Little-endian hash target, formatted as a hexadecimal string.
             , hdTarget  :: HexString
             , hash1     :: HexString
             , midstate  :: HexString
             }
    deriving ( Show, Read, Ord, Eq )

instance FromJSON HashData where
    parseJSON (Object o) = HashData <$> o .: "data"
                                    <*> o .: "target"
                                    <*> o .: "hash1"
                                    <*> o .: "midstate"
    parseJSON _ = mzero

instance ToJSON HashData where
    toJSON (HashData dat tar has mid) = object ["data" .= dat, "target" .= tar, "hash1" .= has, "midstate" .= mid]

-- | Returns formatted hash data to work on.
getWork :: Client -> IO HashData
getWork client = callApi client "getwork" []

-- | Tries to solve the given block, and returns true if it was successful.
solveBlock :: Client -> HexString -> IO Bool
solveBlock client data_ = callApi client "getwork" [ tj data_ ]

-- | A transaction to be included in the next block.
data Transaction =
    Transaction { txnData :: HexString
                , txnHash :: HexString
                , depends :: Vector Integer
                , txnFee  :: Maybe Integer
                , sigOps  :: Integer
                }
    deriving ( Show, Read, Ord, Eq )

instance FromJSON Transaction where
    parseJSON (Object o) = Transaction <$> o .:  "data"
                                       <*> o .:  "hash"
                                       <*> o .:  "depends"
                                       <*> o .:? "fee"
                                       <*> o .:  "sigops"
    parseJSON _ = mzero

newtype CoinBaseAux = CoinBaseAux { cbFlags :: HexString }
    deriving ( Show, Read, Ord, Eq )

instance FromJSON CoinBaseAux where
    parseJSON (Object o) = CoinBaseAux <$> o .: "flags"
    parseJSON _          = mzero

-- | A template for constructing a block to work on.
--
--   See <https://en.bitcoin.it/wiki/BIP_0022> for the full specification.
data BlockTemplate =
    BlockTemplate { blockVersion          :: Integer
                  -- | Hash of current highest block.
                  , previousBlockHash     :: HexString
                  -- | Contents of non-coinbase transactions that should be
                  --   included in the next block.
                  , transactionsToInclude :: Vector Transaction
                  -- | Data that should be included in coinbase.
                  , coinBaseAux           :: CoinBaseAux
                  -- | Maximum allowable input to coinbase transaction,
                  --   including the generation award and transaction fees.
                  , coinBaseValue         :: Integer
                  -- | Hash target.
                  , btTarget              :: HexString
                  -- | Minimum timestamp appropriate for next block.
                  , minTime               :: Integer
                  -- | Range of valid nonces.
                  , nonceRange            :: HexString
                  -- | Limit of sigops in blocks.
                  , sigopLimit            :: Integer
                  -- | Limit of block size.
                  , sizeLimit             :: Integer
                  -- | Current timestamp.
                  , curTime               :: Integer
                  -- | Compressed target of the next block.
                  , btBits                :: HexString
                  -- | Height of the next block.
                  , btHeight              :: Integer
                  }
    deriving ( Show, Read, Ord, Eq )

instance FromJSON BlockTemplate where
    parseJSON (Object o) = BlockTemplate <$> o .: "version"
                                         <*> o .: "previousblockhash"
                                         <*> o .: "transactions"
                                         <*> o .: "coinbaseaux"
                                         <*> o .: "coinbasevalue"
                                         <*> o .: "target"
                                         <*> o .: "mintime"
                                         <*> o .: "noncerange"
                                         <*> o .: "sigoplimit"
                                         <*> o .: "sizelimit"
                                         <*> o .: "curtime"
                                         <*> o .: "bits"
                                         <*> o .: "height"
    parseJSON _ = mzero

-- | Returns data needed to construct a block to work on.
getBlockTemplate :: Client -> IO BlockTemplate
getBlockTemplate client = callApi client "getblocktemplate" []

-- | Unfortunately, the submitblock API call returns null on success, and
--   the string "rejected" on failure.
--
--   We use 'StupidReturnValue' to parse this ridiculous API.
newtype StupidReturnValue = SRV { unStupid :: Bool }

instance FromJSON StupidReturnValue where
    parseJSON Null = return $ SRV True
    parseJSON _    = return $ SRV False

-- | Attempts to submit a new block to the network.
submitBlock :: Client
            -> HexString -- ^ The block to submit.
            -> IO Bool -- ^ Was the block accepted by the network?
submitBlock client block = unStupid <$> callApi client "submitblock" [ tj block ]
