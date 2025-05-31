{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS_GHC -Wall #-}
-- | An interface to bitcoind's available raw transaction-related RPC calls.
--   The implementation of these functions can be found at
--   <https://github.com/bitcoin/bitcoin/blob/master/src/rpcrawtransaction.cpp>.
--
--   If any APIs are missing, patches are always welcome. If you look at the
--   source of this module, you'll see that the interface code is trivial.
--
--   Also, documentation for this module is scarce. I would love the addition
--   of more documentation by anyone who knows what these things are.
module Network.Bitcoin.RawTransaction ( Client
                                      , getClient
                                      , RawTransaction
                                      , getRawTransaction
                                      , TxIn(..)
                                      , TxnOutputType(..)
                                      , ScriptPubKey(..)
                                      , ScriptSig(..)
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
                                      , DecodedPsbt (..)
                                      , signRawTransaction
                                      , sendRawTransaction
                                      , decodePsbt
                                      , joinPsbts
                                      ) where

import           Control.Applicative
import           Control.Monad
import           Data.Aeson               as A
import           Data.Aeson.Types         as AT
import           Data.Maybe
import qualified Data.Vector              as V
import           Network.Bitcoin.Internal

-- | Just like most binary data retrieved from bitcoind, a raw transaction is
--   represented by a hexstring.
--
--   This is a serialized, hex-encoded transaction.
type RawTransaction = HexString

-- | Get a raw transaction from its unique ID.
getRawTransaction :: Client -> TransactionID -> IO RawTransaction
getRawTransaction client (TransactionID txid) =
    callApi client "getrawtransaction" [ tj txid, tj verbose ]
        where verbose = False

-- | A transaction into an account. This can either be a coinbase transaction,
--   or a standard transaction with another account.
data TxIn = TxCoinbase { txCoinbase :: HexString
                       }
          | TxIn { -- | This transaction's ID.
                   txInId     :: TransactionID
                 , numOut     :: Integer
                 , scriptSig  :: ScriptSig
                 -- | A transaction sequence number.
                 , txSequence :: Integer
                 }
    deriving ( Show, Read, Ord, Eq )

instance FromJSON TxIn where
    parseJSON (Object o) = parseCB <|> parseTxIn
        where
            parseCB = TxCoinbase <$> o .: "coinbase"
            parseTxIn = TxIn <$> o .: "txid"
                             <*> o .: "vout"
                             <*> o .: "scriptSig"
                             <*> o .: "sequence"
    parseJSON _ = mzero

-- | The type of a transaction out.
--
--   More documentation is needed here. Submit a patch if you know what this is
--   about!
data TxnOutputType = TxnPubKey     -- ^ JSON of "pubkey" received.
                   | TxnPubKeyHash -- ^ JSON of "pubkeyhash" received.
                   | TxnScriptHash -- ^ JSON of "scripthash" received.
                   | TxnMultisig   -- ^ JSON of "multisig" received.
                   | TxnUnknown
    deriving ( Show, Read, Ord, Eq )

instance FromJSON TxnOutputType where
    parseJSON (A.String s) | s == "pubkey"     = return TxnPubKey
                           | s == "pubkeyhash" = return TxnPubKeyHash
                           | s == "scripthash" = return TxnScriptHash
                           | s == "multisig"   = return TxnMultisig
                           | otherwise         = return TxnUnknown
    parseJSON _ = mzero


-- | A transaction out of an account.
data TxOut =
    TxOut { -- | The amount of bitcoin transferred out.
            txoutVal     :: BTC
          , txoutNum     :: Integer
          -- | The public key of the account we sent the money to.
          , scriptPubKey :: ScriptPubKey
          }
    deriving ( Show, Read, Ord, Eq )

instance FromJSON TxOut where
    parseJSON (Object o) = TxOut <$> o .: "value"
                                 <*> o .: "n"
                                 <*> o .: "scriptPubKey"
    parseJSON _ = mzero

-- * Scripts
--   A script is a complex bitcoin construct that provides the creation
--   of Contracts.
--   See <https://en.bitcoin.it/wiki/Script> and <https://en.bitcoin.it/wiki/Contracts>.
--   It consists of two parts - a public key and a signature.

-- | A public key of someone we sent money to.
data ScriptPubKey = NonStandardScriptPubKey { -- | The JSON "asm" field.
                                              nspkAsm :: HexString
                                              -- | The JSON "hex" field.
                                            , nspkHex :: HexString
                                            }
                  | StandardScriptPubKey { -- | The JSON "asm" field.
                                           sspkAsm       :: HexString
                                         -- | The JSON "hex" field.
                                         , sspkHex       :: HexString
                                         -- | The number of required signatures.
                                         , requiredSigs  :: Integer
                                         -- | The type of the transaction.
                                         , sspkType      :: TxnOutputType
                                         -- | The addresses associated with this key.
                                         , sspkAddresses :: Vector Address
                                         }
                  | StandardScriptPubKeyV22 { -- | The JSON "asm" field.
                                           sspkAsm       :: HexString
                                         -- | The JSON "hex" field.
                                         , sspkHex       :: HexString
                                         -- | The number of required signatures.
                                         , sspkType      :: TxnOutputType
                                         -- | The addresses associated with this key.
                                         , sspkAddress   :: Address
                                         }

    deriving ( Show, Read, Ord, Eq )

instance FromJSON ScriptPubKey where
    parseJSON (Object o) = parseStandard <|> parseStandardV22 <|> parseNonstandard
        where
            parseStandard = StandardScriptPubKey <$> o .: "asm"
                                                 <*> o .: "hex"
                                                 <*> o .: "reqSigs"
                                                 <*> o .: "type"
                                                 <*> o .: "addresses"
            parseStandardV22 = StandardScriptPubKeyV22
                                                 <$> o .: "asm"
                                                 <*> o .: "hex"
                                                 <*> o .: "type"
                                                 <*> o .: "address"
            parseNonstandard = NonStandardScriptPubKey <$> o .: "asm"
                                                       <*> o .: "hex"
    parseJSON _ = mzero

-- | A script signature.
data ScriptSig = ScriptSig { sigAsm :: HexString
                           , sigHex :: HexString
                           }
    deriving ( Show, Read, Ord, Eq )

instance FromJSON ScriptSig where
    parseJSON (Object o) = ScriptSig <$> o .: "asm"
                                     <*> o .: "hex"
    parseJSON _ = mzero


-- | Information on a single block.
data BlockInfo = ConfirmedBlock { -- | The number of confirmations a block has.
                                  --   This will always be >= 1.
                                  confirmations :: Integer
                                --   The JSON "time" field".
                                , cbTime        :: Integer
                                -- | The JSON "blocktime" field.
                                , blockTime     :: Integer
                                }
               | UnconfirmedBlock
               -- ^ An unconfirmed block is boring, but a possibility.
    deriving ( Show, Read, Ord, Eq )

instance FromJSON BlockInfo where
    parseJSON (Object o) = parseConfirmed <|> parseUnconfirmed
        where
            parseConfirmed = ConfirmedBlock <$> o .: "confirmations"
                                            <*> o .: "time"
                                            <*> o .: "blocktime"
            parseUnconfirmed = do c <- o .: "confirmations" :: AT.Parser Integer
                                  guard $ c == 0
                                  return UnconfirmedBlock
    parseJSON _ = mzero

-- | The raw transaction info for a given transaction ID.
data RawTransactionInfo =
    RawTransactionInfo { -- | The raw transaction.
                         raw            :: RawTransaction
                       -- | The transaction version number.
                       , txnVersion     :: Integer
                       , txnLockTime    :: Integer
                       -- | The vector of transactions in.
                       , vin            :: Vector TxIn
                       -- | The vector of transactions out.
                       , vout           :: Vector TxOut
                       -- | The hash of the block that was used for this
                       --   transaction.
                       , rawTxBlockHash :: HexString
                       -- | The transaction's block's info.
                       , rawBlockInfo   :: BlockInfo
                       }
    deriving ( Show, Read, Ord, Eq )

instance FromJSON RawTransactionInfo where
    parseJSON v@(Object o) = RawTransactionInfo <$> o .: "hex"
                                                <*> o .: "version"
                                                <*> o .: "locktime"
                                                <*> o .: "vin"
                                                <*> o .: "vout"
                                                <*> o .: "blockhash"
                                                <*> parseJSON v
    parseJSON _ = mzero

-- | Get raw transaction info for a given transaction ID. The data structure
--   returned is quite sprawling and undocumented, so any patches to help
--   simplify things would be greatly appreciated.
getRawTransactionInfo :: Client -> TransactionID -> IO RawTransactionInfo
getRawTransactionInfo client (TransactionID txid) =
    callApi client "getrawtransaction" [ tj txid, tj verbose ]
        where verbose = True

data UnspentTransaction =
    UnspentTransaction { unspentTransactionId :: TransactionID
                       , outIdx               :: Integer
                       , unspentAddress       :: Address
                       , unspentScriptPubKey  :: HexString
                       , redeemScript         :: Maybe HexString
                       , unspentAmount        :: BTC
                       , usConfirmations      :: Integer
                       } deriving ( Show, Eq )

instance FromJSON UnspentTransaction where
    parseJSON (Object o) = UnspentTransaction <$> o .:  "txid"
                                              <*> o .:  "vout"
                                              <*> o .:  "address"
                                              <*> o .:  "scriptPubKey"
                                              <*> o .:? "redeemScript"
                                              <*> o .:  "amount"
                                              <*> o .:  "confirmations"
    parseJSON _ = mzero

-- Instance used in 'createRawTransaction'.
instance ToJSON UnspentTransaction where
    toJSON UnspentTransaction{..} = object [ "txid" .= unspentTransactionId
                                           , "vout" .= outIdx
                                           ]

-- | Returns an array of unspent transaction outputs with between minconf and
--   maxconf (inclusive) confirmations. If addresses are given, the result will
--   be filtered to include only those addresses.
listUnspent :: Client
            -> Maybe Int -- ^ minconf. Defaults to 1 if 'Nothing'.
            -> Maybe Int -- ^ maxconf. Defaults to 9999999 if 'Nothing'.
            -> Vector Address -- ^ Use 'Data.Vector.empty' for no filtering.
            -> IO (Vector UnspentTransaction)
listUnspent client mmin mmax vaddrs =
    let min' = fromMaybe 1 mmin
        max' = fromMaybe 9999999 mmax
     in callApi client "listunspent" [ tj min', tj max', tj vaddrs ]

-- | Create a transaction spending given inputs, sending to given addresses.
--
--   Note that the transaction's inputs are not signed, and it is not stored
--   in the wallet or transmitted to the network.
--
--   Also, there is no checking to see if it's possible to send that much to
--   the targets specified. In the future, such a scenario might throw an
--   exception.
createRawTransaction :: Client
                     -> Vector UnspentTransaction
                     -- ^ The unspent transactions we'll be using as our output.
                     -> Vector (Address, BTC)
                     -- ^ The addresses we're sending money to, along with how
                     --   much each of them gets.
                     -> IO HexString
createRawTransaction client us tgts =
    callApi client "createrawtransaction" [ tj us, tj $ AA tgts ]

-- | A successfully decoded raw transaction, from a given serialized,
--   hex-encoded transaction.
data DecodedRawTransaction =
    DecodedRawTransaction {
                          -- | The transaction version number.
                            decTxnVersion  :: Integer
                          , decTxId        :: TransactionID
                          , decTxnLockTime :: Integer
                          , decSize        :: Integer
                          , decVsize       :: Integer
                          -- | The vector of transactions in.
                          , decVin         :: Vector TxIn
                          -- | The vector of transactions out.
                          , decVout        :: Vector TxOut
                          } deriving (Show, Read, Ord, Eq)

instance FromJSON DecodedRawTransaction where
    parseJSON (Object o) = DecodedRawTransaction <$> o .: "version"
                                                 <*> o .: "txid"
                                                 <*> o .: "locktime"
                                                 <*> o .: "size"
                                                 <*> o .: "vsize"
                                                 <*> o .: "vin"
                                                 <*> o .: "vout"
    parseJSON _ = mzero

-- | Decodes a raw transaction into a more accessible data structure.
decodeRawTransaction :: Client -> RawTransaction -> IO DecodedRawTransaction
decodeRawTransaction client tx = callApi client "decoderawtransaction" [ tj tx ]

-- | Used internally to give a new 'ToJSON' instance for 'UnspentTransaction'.
newtype UnspentForSigning = UFS UnspentTransaction

instance ToJSON UnspentForSigning where
    toJSON (UFS UnspentTransaction{..})
        | isNothing redeemScript =
            object [ "txid" .= unspentTransactionId
                   , "vout" .= outIdx
                   , "scriptPubKey" .= unspentScriptPubKey
                   ]
        | otherwise =
            object [ "txid" .= unspentTransactionId
                   , "vout" .= outIdx
                   , "scriptPubKey" .= unspentScriptPubKey
                   , "redeemScript" .= fromJust redeemScript
                   ]

-- | Who can pay for a given transaction.
data WhoCanPay = All
               | AllOrAnyoneCanPay
               | None
               | NoneOrAnyoneCanPay
               | Single
               | SingleOrAnyoneCanPay

toString :: WhoCanPay -> Text
toString All                  = "ALL"
toString AllOrAnyoneCanPay    = "ALL|ANYONECANPAY"
toString None                 = "NONE"
toString NoneOrAnyoneCanPay   = "NONE|ANYONECANPAY"
toString Single               = "SINGLE"
toString SingleOrAnyoneCanPay = "SINGLE|ANYONECANPAY"

-- | A raw signed transaction contains the raw, signed hexstring and whether or
--   not this transaction has a complete signature set.
data RawSignedTransaction =
    RawSignedTransaction { rawSigned         :: HexString
                         , hasCompleteSigSet :: Bool
                         }

instance FromJSON RawSignedTransaction where
    parseJSON (Object o) = RawSignedTransaction <$> o .: "hex"
                                                <*> o .: "complete"
    parseJSON _ = mzero

-- | Sign inputs for a raw transaction.
signRawTransaction :: Client
                   -> RawTransaction
                   -- ^ The raw transaction whose inputs we're signing.
                   -> Maybe (Vector UnspentTransaction)
                   -- ^ An optional list of previous transaction outputs that
                   --   this transaction depends on but may not yet be in the
                   --   block chain.
                   -> Maybe (Vector HexString)
                   -- ^ An array of base58-encoded private keys that, if given,
                   --   will be the only keys used to sign the transaction.
                   -> Maybe WhoCanPay
                   -- ^ Who can pay for this transaction? 'All' by default.
                   -> IO RawSignedTransaction
                   -- ^ Returns 'Nothing' if the transaction has a complete set
                   --   of signatures, and the raw signed transa
signRawTransaction client rt us' privkeys wcp =
    let us = V.map UFS <$> us' :: Maybe (Vector UnspentForSigning)
     in callApi client "signrawtransaction" [ tj rt
                                          , tj us
                                          , tj privkeys
                                          , tj . toString $ fromMaybe All wcp
                                          ]

sendRawTransaction :: Client -> RawTransaction -> IO TransactionID
sendRawTransaction client rt = callApi client "sendrawtransaction" [ tj rt ]

newtype DecodedPsbt = DecodedPsbt {
    tx :: DecodedRawTransaction
} deriving ( Show, Read, Ord, Eq )

instance FromJSON DecodedPsbt where
    parseJSON (Object o) = DecodedPsbt <$> o .: "tx"
    parseJSON _ = mzero

decodePsbt :: Client -> HexString -> IO DecodedPsbt
decodePsbt client psbt = callApi client "decodepsbt" [ tj psbt ]

joinPsbts :: Client -> [HexString] -> IO HexString
joinPsbts client psbts = callApi client "joinpsbts" [ tj psbts ]
