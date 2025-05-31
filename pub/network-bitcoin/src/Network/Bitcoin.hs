{-# OPTIONS_GHC -Wall #-}
-- | A Haskell binding to the bitcoind server.
module Network.Bitcoin
    (
    -- * Common Types
      Client
    , getClient
    , BitcoinException(..)
    , HexString
    , TransactionID
    , Satoshi(..)
    , BTC
    , Account
    , Address
    , ScriptSig
    -- * Block Chain Operations
    , getBlockCount
    , getDifficulty
    , setTransactionFee
    , getRawMemoryPool
    , BlockHash
    , getBlockHash
    , Block(..)
    , BlockVerbose(..)
    , getBlock
    , getBlockVerbose
    , OutputSetInfo(..)
    , getOutputSetInfo
    , OutputInfo(..)
    , getOutputInfo
    -- * Private Key Operations
    , importPrivateKey
    , dumpPrivateKey
    -- * Mining Operations
    , generate
    , generateToAddress
    , getGenerate
    , setGenerate
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
    -- * Network Operations
    , getConnectionCount
    , PeerInfo(..)
    , getPeerInfo
    , AddNodeCommand(..)
    , addNode
    , disconnectNode
    , setNetworkActive
    -- * Raw Transaction Operations
    , RawTransaction
    , getRawTransaction
    , TxIn(..)
    , TxnOutputType(..)
    , ScriptPubKey(..)
    , TxOut(..)
    , BlockInfo(..)
    , RawTransactionInfo(..)
    , getRawTransactionInfo
    , UnspentTransaction(..)
    , listUnspent
    , createRawTransaction
    , DecodedRawTransaction(..)
    , decodeRawTransaction
    , WhoCanPay(..)
    , RawSignedTransaction(..)
    , signRawTransaction
    , sendRawTransaction
    -- * Wallet Operations
    , BitcoindInfo(..)
    , getBitcoindInfo
    , getNewAddress
    , getAccountAddress
    , getAccount
    , setAccount
    , getAddressesByAccount
    , sendToAddress
    , AddressInfo(..)
    , listAddressGroupings
    , Signature
    , signMessage
    , verifyMessage
    , getReceivedByAddress
    , getReceivedByAddress'
    , getReceivedByAccount
    , getReceivedByAccount'
    , getBalance
    , getBalance'
    , getBalance''
    , moveBitcoins
    , sendFromAccount
    , sendMany
    , EstimationMode (..)
    , estimateSmartFee
    -- , createMultiSig
    , ReceivedByAddress(..)
    , listReceivedByAddress
    , listReceivedByAddress'
    , ReceivedByAccount(..)
    , listReceivedByAccount
    , listReceivedByAccount'
    , listTransactions
    , listTransactions'
    , listAccounts
    , importAddress
    , SinceBlock(..)
    , SimpleTransaction(..)
    , TransactionCategory(..)
    , listSinceBlock
    , listSinceBlock'
    , DetailedTransaction(..)
    , DetailedTransactionDetails(..)
    , getTransaction
    , backupWallet
    , keyPoolRefill
    , unlockWallet
    , lockWallet
    , changePassword
    , encryptWallet
    , isAddressValid
    , DecodedPsbt (..)
    ) where

import           Network.Bitcoin.BlockChain
import           Network.Bitcoin.Dump
import           Network.Bitcoin.Mining
import           Network.Bitcoin.Net
import           Network.Bitcoin.RawTransaction
import           Network.Bitcoin.Types
import           Network.Bitcoin.Wallet
