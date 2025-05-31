{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
-- | An interface to bitcoind's available block-chain-related RPC calls. The
--   implementation of these functions can be found at
--   <https://github.com/bitcoin/bitcoin/blob/master/src/rpcblockchain.cpp>.
--
--   If any APIs are missing, patches are always welcome. If you look at the
--   source of this module, you'll see that the interface code is trivial.
module Network.Bitcoin.BlockChain ( Client
                                  , TransactionID
                                  , BTC
                                  , getBlockCount
                                  , getDifficulty
                                  , setTransactionFee
                                  , getRawMemoryPool
                                  , BlockHash
                                  , BlockHeight
                                  , getBlockHash
                                  , Block(..)
                                  , BlockVerbose(..)
                                  , getBlock
                                  , getBlockVerbose
                                  , OutputSetInfo(..)
                                  , getOutputSetInfo
                                  , OutputInfo(..)
                                  , getOutputInfo
                                  , BlockChainInfo(..)
                                  , getBlockChainInfo
                                  ) where

import           Control.Monad
import           Data.Aeson
import           Network.Bitcoin.Internal
import           Network.Bitcoin.RawTransaction

-- | Returns the number of blocks in the longest block chain.
getBlockCount :: Client -> IO BlockHeight
getBlockCount client = callApi client "getblockcount" []

-- | Returns the proof-of-work difficulty as a multiple of the minimum
--   difficulty.
getDifficulty :: Client -> IO Integer
getDifficulty client = callApi client "getdifficulty" []

-- | Sets the transaction fee will will pay to the network. Values of 0 are
--   rejected.
setTransactionFee :: Client -> BTC -> IO ()
setTransactionFee client fee =
    stupidAPI <$> callApi client "settxfee" [ tj fee ]
        where stupidAPI :: Bool -> ()
              stupidAPI = const ()

-- | Returns all transaction identifiers in the memory pool.
getRawMemoryPool :: Client -> IO (Vector TransactionID)
getRawMemoryPool client = callApi client "getrawmempool" []

-- | The hash of a given block.
type BlockHash = HexString
type BlockHeight = Integer

-- | Returns the hash of the block in best-block-chain at the given index.
getBlockHash :: Client
             -> BlockHeight -- ^ Block index.
             -> IO BlockHash
getBlockHash client idx = callApi client "getblockhash" [ tj idx ]

-- | Information about a given block in the block chain.
data Block = Block { blockHash        :: BlockHash
                   -- | The number of confirmations the block has.
                   , blkConfirmations :: Integer
                   -- | The size of the block.
                   , blkSize          :: Integer
                   -- | The "height" of the block. TODO: Clarify this.
                   , blkHeight        :: BlockHeight
                   -- | The version of the block.
                   , blkVersion       :: Integer
                   -- | The hash of the block at the root of the merkle tree
                   --   which this block belongs to.
                   , merkleRoot       :: BlockHash
                   -- | Should this be a transaction, or transaction id?
                   , subTransactions  :: Vector TransactionID
                   -- | The time it was mined.
                   , blkTime          :: Integer
                   -- | The block's nonce.
                   , blkNonce         :: Integer
                   , blkBits          :: HexString
                   -- | How hard was this block to mine?
                   , blkDifficulty    :: Double
                   -- | A pointer to the next block in the chain.
                   , nextBlock        :: Maybe BlockHash
                   -- | A pointer to the previous block in the chain.
                   , prevBlock        :: Maybe BlockHash
                   }
    deriving ( Show, Read, Ord, Eq )

data BlockVerbose = BlockVerbose {
    vBlockHash        :: BlockHash
    -- | The number of confirmations the block has.
    , vBlkConfirmations :: Integer
    -- | The size of the block.
    , vBlkSize          :: Integer
    -- | The "height" of the block. TODO: Clarify this.
    , vBlkHeight        :: BlockHeight
    -- | The version of the block.
    , vBlkVersion       :: Integer
    -- | The hash of the block at the root of the merkle tree
    --   which this block belongs to.
    , vBerkleRoot       :: BlockHash
    -- | Should this be a transaction, or transaction id?
    , vSubTransactions  :: Vector DecodedRawTransaction
    -- | The time it was mined.
    , vBlkTime          :: Integer
    -- | The block's nonce.
    , vBlkNonce         :: Integer
    , vBlkBits          :: HexString
    -- | How hard was this block to mine?
    , vBlkDifficulty    :: Double
    -- | A pointer to the next block in the chain.
    , vNextBlock        :: Maybe BlockHash
    -- | A pointer to the previous block in the chain.
    , vPrevBlock        :: Maybe BlockHash
    }
    deriving ( Show, Read, Ord, Eq )



-- Missing from data type:
-- - strippedsize
-- - weight
-- - versionHex
-- - mediantime
-- - nTx
-- - chainwork
instance FromJSON Block where
    parseJSON (Object o) = Block <$> o .:  "hash"
                                 <*> o .:  "confirmations"
                                 <*> o .:  "size"
                                 <*> o .:  "height"
                                 <*> o .:  "version"
                                 <*> o .:  "merkleroot"
                                 <*> o .:  "tx"
                                 <*> o .:  "time"
                                 <*> o .:  "nonce"
                                 <*> o .:  "bits"
                                 <*> o .:  "difficulty"
                                 <*> o .:? "nextblockhash"
                                 <*> o .:? "previousblockhash"
    parseJSON _ = mzero

instance FromJSON BlockVerbose where
    parseJSON (Object o) = BlockVerbose <$> o .:  "hash"
                                 <*> o .:  "confirmations"
                                 <*> o .:  "size"
                                 <*> o .:  "height"
                                 <*> o .:  "version"
                                 <*> o .:  "merkleroot"
                                 <*> o .:  "tx"
                                 <*> o .:  "time"
                                 <*> o .:  "nonce"
                                 <*> o .:  "bits"
                                 <*> o .:  "difficulty"
                                 <*> o .:? "nextblockhash"
                                 <*> o .:? "previousblockhash"
    parseJSON _ = mzero

-- | Returns details of a block with given block-hash.
getBlock :: Client -> BlockHash -> IO Block
getBlock client bh = callApi client "getblock" [ tj bh ]


getBlockVerbose :: Client -> BlockHash -> IO BlockVerbose
getBlockVerbose client bh = callApi client "getblock" [ tj bh, tj (2 :: Int)]

-- | Information on the unspent transaction in the output set.
data OutputSetInfo =
    OutputSetInfo { osiBestBlock       :: BlockHash
                  -- | The height of the best block chain
                  , osiHeight          :: BlockHeight
                  -- | The number of transactions in the output set.
                  , numTransactions    :: Integer
                  -- | The number of outputs for the transactions.
                  , transactionOutputs :: Integer
                  -- | The serialized size of the output set.
                  , serializedSize     :: Integer
                  }
    deriving ( Show, Read, Ord, Eq )

instance FromJSON OutputSetInfo where
    parseJSON (Object o) = OutputSetInfo <$> o .: "bestblock"
                                         <*> o .: "height"
                                         <*> o .: "transactions"
                                         <*> o .: "txouts"
                                         <*> o .: "bytes_serialized"
    parseJSON _ = mzero

-- | Returns statistics about the unspent transaction output set.
getOutputSetInfo :: Client -> IO OutputSetInfo
getOutputSetInfo client = callApi client "gettxoutsetinfo" []

-- | Details about an unspent transaction output.
data OutputInfo =
    OutputInfo { oiBestBlock     :: BlockHash
               -- | The number of times this transaction has been confirmed.
               , oiConfirmations :: Integer
               -- | The amount transferred.
               , oiAmount        :: BTC
               -- | The public key of the sender.
               , oiScriptPubKey  :: ScriptPubKey
               -- | The version of this transaction.
               , oiVersion       :: Maybe Integer
               -- | Is this transaction part of the coin base?
               , oiCoinBase      :: Bool
               }
    deriving ( Show, Read, Ord, Eq )

instance FromJSON OutputInfo where
    parseJSON (Object o) = OutputInfo <$> o .: "bestblock"
                                      <*> o .: "confirmations"
                                      <*> o .: "value"
                                      <*> o .: "scriptPubKey"
                                      <*> o .:? "version"
                                      <*> o .: "coinbase"
    parseJSON _ = mzero

-- | Returns details about an unspent transaction output.
getOutputInfo :: Client
              -> TransactionID
              -> Integer -- ^ The index we're looking at.
              -> IO (Maybe OutputInfo)
getOutputInfo client txid n = callApi client "gettxout" [ tj txid, tj n ]

-- | Details about blockchain.
data BlockChainInfo =
    BlockChainInfo { bciChain :: Text
                   , bciBlocks :: BlockHeight
                   , bciHeaders :: BlockHeight
                   , bciInitialBlockDownload :: Bool
                   }
    deriving ( Show, Read, Ord, Eq )

instance FromJSON BlockChainInfo where
    parseJSON (Object o) = BlockChainInfo
                            <$> o .: "chain"
                            <*> o .: "blocks"
                            <*> o .: "headers"
                            <*> o .: "initialblockdownload"
    parseJSON _ = mzero

-- | Returns details about blockchain.
getBlockChainInfo :: Client -> IO BlockChainInfo
getBlockChainInfo client =
  callApi client "getblockchaininfo" []
