{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall #-}

-- | An interface to bitcoind's available wallet-related RPC calls.
--   The implementation of these functions can be found at
--   <https://github.com/bitcoin/bitcoin/blob/master/src/rpcwallet.cpp>.
--
--   If any APIs are missing, patches are always welcome. If you look at the
--   source of this module, you'll see that the interface code is trivial.
--
--   Certain APIs were too complicated for me to write an interface for. If
--   you figure them out, then patches are always welcome! They're left in
--   the source as comments.
module Network.Bitcoin.Wallet ( Client
                              , getClient
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
                              , getAddrInfo
                              , AddrInfo(..)
                              , ScrPubKey(..)
                              , WalletCfg (..)
                              , WalletRes (..)
                              , defaultWalletCfg
                              , defaultAutoLoadWalletCfg
                              , createWallet
                              , loadWallet
                              ) where

import           Control.Exception              (throw)
import           Control.Monad
import           Data.Aeson                     as A
import           Data.Aeson.Types               (parseEither)
import qualified Data.ByteString.Lazy.Char8     as BSL8
import qualified Data.HashMap.Lazy              as HM
import           Data.Maybe
import           Data.Text
import           Data.Time.Clock.POSIX
import           Data.Vector                    as V hiding ((++))
import           Data.Word
import           Network.Bitcoin.BlockChain     (BlockHash)
import           Network.Bitcoin.Internal
import           Network.Bitcoin.RawTransaction (RawTransaction)


-- | A plethora of information about a bitcoind instance.
data BitcoindInfo = BitcoindInfo
  { -- | What version of bitcoind are we running?
    bitcoinVersion :: Integer,
    -- | What is bitcoind's current protocol number?
    protocolVersion :: Integer,
    -- | What version is the wallet?
    walletVersion :: Integer,
    -- | How much money is currently in the wallet?
    balance :: BTC,
    -- | The number of blocks in our chain.
    numBlocks :: Integer,
    -- | How many peers are we connected to?
    numConnections :: Integer,
    -- | A blank string if we're not using a proxy.
    proxy :: Text,
    -- | The difficulty multiplier for bitcoin mining operations.
    generationDifficulty :: Double,
    -- | Are we on the test network (as opposed to the primary
    --   bitcoin network)?
    onTestNetwork :: Bool,
    -- | The timestamp of the oldest key in the key pool.
    keyPoolOldest :: Integer,
    -- | The size of the key pool.
    keyPoolSize :: Integer,
    -- | How much do we currently pay as a transaction fee?
    transactionFeePaid :: BTC,
    -- | If the wallet is unlocked, the number of seconds until a
    --   re-lock is needed.
    unlockedUntil :: Maybe Integer,
    -- | Any alerts will show up here. This should normally be an
    --   empty string.
    bitcoindErrors :: Text
  }
  deriving (Show, Read, Ord, Eq)

instance FromJSON BitcoindInfo where
  parseJSON (Object o) =
    BitcoindInfo <$> o .: "version"
      <*> o .: "protocolversion"
      <*> o .: "walletversion"
      <*> o .: "balance"
      <*> o .: "blocks"
      <*> o .: "connections"
      <*> o .: "proxy"
      <*> o .: "difficulty"
      <*> o .: "testnet"
      <*> o .: "keypoololdest"
      <*> o .: "keypoolsize"
      <*> o .: "paytxfee"
      <*> o .:? "unlocked_until"
      <*> o .: "errors"
  parseJSON _ = mzero

-- | Returns an object containing various state info.
--
-- /Availability:/ @< 0.16@
getBitcoindInfo :: Client -> IO BitcoindInfo
getBitcoindInfo client = callApi client "getinfo" []

-- | Returns a new bitcoin address for receiving payments.
--
--   If an account is specified (recommended), the new address is added to the
--   address book so payments received with the address will be credited to the
--   given account.
--
--   If no account is specified, the address will be credited to the account
--   whose name is the empty string. i.e. the default account.
getNewAddress :: Client -> Maybe Account -> IO Address
getNewAddress client ma =
  let acc = fromMaybe "" ma
   in callApi client "getnewaddress" [tj acc]

-- | Returns the current Bitcoin address for receiving payments to the given
--   account.
getAccountAddress :: Client -> Account -> IO Address
getAccountAddress client acc = callApi client "getaccountaddress" [tj acc]

-- | Sets the account associated with the given address.
setAccount :: Client -> Address -> Account -> IO ()
setAccount client addr acc = unNil <$> callApi client "setaccount" [tj addr, tj acc]

-- | Returns the account associated with the given address.
getAccount :: Client -> Address -> IO Account
getAccount client addr = callApi client "getaccount" [tj addr]

-- | Returns the list of addresses for the given address.
getAddressesByAccount :: Client -> Account -> IO (Vector Address)
getAddressesByAccount client acc = callApi client "getaddressesbyaccount" [tj acc]

-- | Sends some bitcoins to an address.
sendToAddress ::
  Client ->
  -- | Who we're sending to.
  Address ->
  -- | The amount to send.
  BTC ->
  -- | An optional comment for the transaction.
  Maybe Text ->
  -- | An optional comment-to (who did we sent this to?) for the
  --   transaction.
  Maybe Text ->
  IO TransactionID
sendToAddress client addr amount comm comm2 =
  callApi client "sendtoaddress" [tj addr, tj amount, tj comm, tj comm2]

-- | Information on a given address.
data AddressInfo = AddressInfo
  { -- | The address in question.
    aiAddress :: Address,
    -- | The address' balance.
    aiAmount :: BTC,
    -- | The address' linked account.
    aiAccount :: Maybe Account
  }
  deriving (Show, Read, Eq, Ord)

-- | What a silly API.
instance FromJSON AddressInfo where
  parseJSON (A.Array a)
    | V.length a == 2 =
      AddressInfo <$> parseJSON (a ! 0)
        <*> parseJSON (a ! 1)
        <*> pure Nothing
    | V.length a == 3 =
      AddressInfo <$> parseJSON (a ! 0)
        <*> parseJSON (a ! 1)
        <*> (Just <$> parseJSON (a ! 2))
    | otherwise = mzero
  parseJSON _ = mzero

-- | Lists groups of addresses which have had their common ownership made
--   public by common use as inputs or as the resulting change in past
--   transactions.
listAddressGroupings ::
  Client ->
  IO (Vector (Vector AddressInfo))
listAddressGroupings client =
  callApi client "listaddressgroupings" []

-- | A signature is a base-64 encoded string.
type Signature = HexString

-- | Sign a message with the private key of an address.
signMessage ::
  Client ->
  -- | The address whose private key we'll use.
  Address ->
  -- | The message to sign.
  Text ->
  IO Signature
signMessage client addr msg = callApi client "signmessage" [tj addr, tj msg]

-- | Verifies a signed message.
verifyMessage ::
  Client ->
  -- | The address of the original signer.
  Address ->
  -- | The message's signature.
  Signature ->
  -- | The message.
  Text ->
  -- | Was the signature valid?
  IO Bool
verifyMessage client addr sig msg =
  callApi client "verifymessage" [tj addr, tj sig, tj msg]

-- | Returns the total amount received by the given address with at least one
--   confirmation.
getReceivedByAddress :: Client -> Address -> IO BTC
getReceivedByAddress client addr =
  callApi client "getreceivedbyaddress" [tj addr]

-- | Returns the total amount received by the given address, with at least the
--   give number of confirmations.
getReceivedByAddress' ::
  Client ->
  Address ->
  -- | The minimum number of confirmations needed
  --   for a transaction to to count towards the
  --   total.
  Int ->
  IO BTC
getReceivedByAddress' client addr minconf =
  callApi client "getreceivedbyaddress" [tj addr, tj minconf]

-- | Returns the total amount received by address with the given account.
getReceivedByAccount :: Client -> Account -> IO BTC
getReceivedByAccount client acc =
  callApi client "getreceivedbyaccount" [tj acc]

-- | Returns the total amount received by addresses with the given account,
--   counting only transactions with the given minimum number of confirmations.
getReceivedByAccount' ::
  Client ->
  -- | The account in question.
  Account ->
  -- | The minimum number of confirmations needed for a
  --   transaction to count towards the total.
  Int ->
  IO BTC
getReceivedByAccount' client acc minconf =
  callApi client "getreceivedbyaccount" [tj acc, tj minconf]

-- | Returns the server's total available balance.
getBalance ::
  Client ->
  IO BTC
getBalance client =
  callApi client "getbalance" []

-- | Returns the balance in the given account, counting only transactions with
--   at least one confirmation.
getBalance' ::
  Client ->
  Account ->
  IO BTC
getBalance' client acc =
  callApi client "getbalance" [tj acc]

-- | Returns the balance in the given account, counting only transactions with
--   at least the given number of confirmations.
getBalance'' ::
  Client ->
  Account ->
  -- | The minimum number of confirmations needed for a transaction
  --   to count towards the total.
  Int ->
  IO BTC
getBalance'' client acc minconf =
  callApi client "getbalance" [tj acc, tj minconf]

-- | Move bitcoins from one account in your wallet to another.
--
--   If you want to send bitcoins to an address not in your wallet, use
--   'sendFromAccount'.
moveBitcoins ::
  Client ->
  -- | From.
  Account ->
  -- | To.
  Account ->
  -- | The amount to transfer.
  BTC ->
  -- | A comment to record for the transaction.
  Text ->
  IO ()
moveBitcoins client from to amt comm =
  stupidAPI <$> callApi client "move" [tj from, tj to, tj amt, tj one, tj comm]
  where
    one = 1 :: Int -- needs a type, else default-integer warnings.
    stupidAPI :: Bool -> ()
    stupidAPI = const ()

-- | Sends bitcoins from a given account in our wallet to a given address.
--
--   A transaction and sender comment may be optionally provided.
sendFromAccount ::
  Client ->
  -- | The account to send from.
  Account ->
  -- | The address to send to.
  Address ->
  -- | The amount to send.
  BTC ->
  -- | An optional transaction comment.
  Maybe Text ->
  -- | An optional comment on who the money is going to.
  Maybe Text ->
  IO TransactionID
sendFromAccount client from to amount comm comm2 =
  callApi client "sendfrom" [tj from, tj to, tj amount, tj one, tj comm, tj comm2]
  where
    one = 1 :: Int -- needs a type, else default-integer warnings.

-- | Send to a whole bunch of address at once.
sendMany ::
  Client ->
  -- | The account to send from.
  Account ->
  -- | The address, and how much to send to each one.
  Vector (Address, BTC) ->
  -- | An optional transaction comment.
  Maybe Text ->
  IO TransactionID
sendMany client acc amounts comm =
  callApi client "sendmany" [tj acc, tj $ AA amounts, tj (1 :: Int), tj comm]

-- TODO: createmultisig.
--
--       I have no idea what this is doing. Patches adding this function are
--       always welcome!

-- | Information on how much was received by a given address.
data ReceivedByAddress = ReceivedByAddress
  { -- | The address which the money was deposited to.
    recvAddress :: Address,
    -- | The account which this address belongs to.
    recvAccount :: Account,
    -- | The amount received.
    recvAmount :: BTC,
    -- | The number of confirmations of the most recent
    --   included transaction.
    recvNumConfirmations :: Integer
  }
  deriving (Show, Read, Ord, Eq)

instance FromJSON ReceivedByAddress where
  parseJSON (Object o) =
    ReceivedByAddress <$> o .: "address"
      <*> o .: "label"
      <*> o .: "amount"
      <*> o .: "confirmations"
  parseJSON _ = mzero

-- | Lists the amount received by each address which has received money at some
--   point, counting only transactions with at least one confirmation.
listReceivedByAddress :: Client -> IO (Vector ReceivedByAddress)
listReceivedByAddress client = listReceivedByAddress' client 1 False

-- | List the amount received by each of our addresses, counting only
--   transactions with the given minimum number of confirmations.
listReceivedByAddress' ::
  Client ->
  -- | The minimum number of confirmations before a
  --   transaction counts toward the total amount
  --   received.
  Int ->
  -- | Should we include addresses with no money
  --   received?
  Bool ->
  IO (Vector ReceivedByAddress)
listReceivedByAddress' client minconf includeEmpty =
  callApi client "listreceivedbyaddress" [tj minconf, tj includeEmpty]

data ReceivedByAccount = ReceivedByAccount
  { -- | The account we received into.
    raccAccount :: Account,
    -- | The mount received.
    -- ^ The number of confirmations of the most recent
    --   included transaction.
    raccAmount :: BTC,
    raccNumConfirmations :: Integer
  }
  deriving (Show, Read, Ord, Eq)

instance FromJSON ReceivedByAccount where
  parseJSON (Object o) =
    ReceivedByAccount <$> o .: "account"
      <*> o .: "amount"
      <*> o .: "confirmations"
  parseJSON _ = mzero

-- | Lists the amount received by each account which has received money at some
--   point, counting only transactions with at leaset one confirmation.
listReceivedByAccount :: Client -> IO (Vector ReceivedByAccount)
listReceivedByAccount client = listReceivedByAccount' client 1 False

-- | List the amount received by each of our accounts, counting only
--   transactions with the given minimum number of confirmations.
listReceivedByAccount' ::
  Client ->
  -- | The minimum number of confirmations before a
  --   transaction counts toward the total received.
  Int ->
  -- | Should we include the accounts with no money
  --   received?
  Bool ->
  IO (Vector ReceivedByAccount)
listReceivedByAccount' client minconf includeEmpty =
  callApi client "listreceivedbyaccount" [tj minconf, tj includeEmpty]

data SinceBlock = SinceBlock
  { strransactions :: Vector SimpleTransaction,
    sbLastBlockHash :: BlockHash
  }
  deriving (Show, Ord, Eq)

instance FromJSON SinceBlock where
  parseJSON (Object o) =
    SinceBlock <$> o .: "transactions"
      <*> o .: "lastblock"
  parseJSON _ = mzero

-- | Data type for simple transactions. Rules involving 'Maybe' are
--   indications of the most probable value only when the transaction is
--   obtained from 'listTransactions' or 'listSinceBlock' are their associated
--   methods. They are never enforced on this side.
data SimpleTransaction = SimpleTransaction
  { -- | The account name associated with the transaction. The empty string
    --   is the default account.
    stReceivingAccount :: Account,
    -- | The bitcoin address of the transaction. Is 'Nothing' unless
    --   'trCategory' is 'TCSend' or 'TCReceive'.
    stAddress :: Maybe Address,
    -- | The category of the transaction
    stCategory :: TransactionCategory,
    -- | The fees paid to process the transaction. Is 'Nothing' unless
    --   'trCategory' is 'TCSend' or 'TCReceive'.
    stFee :: Maybe BTC,
    -- | The amount of bitcoins transferred.
    stAmount :: BTC,
    -- | The number of confirmations of the transaction. Is 'Nothing' unless
    --   'trCategory' is 'TCSend' or 'TCReceive'.
    stConfirmations :: Maybe Integer,
    -- | The hash of the block containing the transaction. Is 'Nothing'
    --   unless 'trCategory' is 'TCSend' or 'TCReceive'.
    stBlockHash :: Maybe BlockHash,
    -- | The index of the the block containing the transaction. Is 'Nothing'
    --   unless  'trCategory' is 'TCSend' or 'TCReceive'.
    stBlockIndex :: Maybe Integer,
    -- | The block time in seconds since epoch (1 Jan 1970 GMT). Is
    --   'Nothing' unless 'trCategory' is 'TCSend' or 'TCReceive'.
    stBlockTime :: Maybe POSIXTime,
    -- | The transaction id. Is 'Nothing' unless
    --   'trCategory' is 'TCSend' or 'TCReceive'.
    stTransactionId :: Maybe TransactionID,
    -- | The list of transaction ids containing the same data as the
    --   original transaction (See ID-malleation bug). Is 'Nothing' unless
    --   'trCategory' is 'TCSend' or 'TCReceive'.
    stWalletConflicts :: Maybe (Vector TransactionID),
    -- | The block time in seconds since epoch (1 Jan 1970 GMT).
    stTime :: POSIXTime,
    stTimeReceived :: Maybe POSIXTime,
    -- | Is 'Nothing' unless a comment is associated with the transaction.
    stComment :: Maybe Text,
    -- | Is 'Nothing' unless a \"to\" is associated with the transaction.
    stTo :: Maybe Text,
    -- | The account the funds came from (for receiving funds, positive
    --   amounts), or went to (for sending funds, negative amounts). Is
    --   'Nothing' unless 'trCategory' is 'TCMove'.
    stOtherAccount :: Maybe Account
  }
  deriving (Show, Ord, Eq)

instance FromJSON SimpleTransaction where
  parseJSON (Object o) =
    SimpleTransaction
      <$> o .: "account"
      <*> o .:? "address"
      <*> o .: "category"
      <*> o .:? "fee"
      <*> o .: "amount"
      <*> o .:? "confirmations"
      <*> o .:? "blockhash"
      <*> o .:? "blockindex"
      <*> (fmap fromInteger <$> o .:? "blocktime")
      <*> o .:? "txid"
      <*> o .:? "walletconflicts"
      <*> (fromInteger <$> o .: "time")
      <*> (fmap fromInteger <$> o .:? "timereceived")
      <*> o .:? "comment"
      <*> o .:? "to"
      <*> o .:? "otheraccount"
  parseJSON _ = mzero

data TransactionCategory
  = TCSend
  | TCOrphan
  | TCImmature
  | TCGenerate
  | TCReceive
  | TCMove
  | TCErrorUnexpected Text
  deriving (Show, Read, Ord, Eq)

instance FromJSON TransactionCategory where
  parseJSON (String s) = return $ createTC s
    where
      createTC :: Text -> TransactionCategory
      createTC "send" = TCSend
      createTC "orphan" = TCOrphan
      createTC "immature" = TCImmature
      createTC "generate" = TCGenerate
      createTC "receive" = TCReceive
      createTC "move" = TCMove
      createTC uc = TCErrorUnexpected uc
  parseJSON _ = mzero

-- | Gets all transactions in blocks since the given block.
listSinceBlock ::
  Client ->
  -- | The hash of the first block to list.
  BlockHash ->
  -- | The minimum number of confirmations before a
  --   transaction can be returned as 'sbLastBlockHash'. This does
  --   not in any way affect which transactions are returned
  --   (see https://github.com/bitcoin/bitcoin/pull/199#issuecomment-1514952)
  Maybe Int ->
  IO SinceBlock
listSinceBlock client blockHash =
  listSinceBlock' client (Just blockHash)

-- | Gets all transactions in blocks since the given block, or all
--   transactions if ommited.
listSinceBlock' ::
  Client ->
  -- | The hash of the first block to list.
  Maybe BlockHash ->
  -- | The minimum number of confirmations before a
  --   transaction can be returned as 'sbLastBlockHash'. This does
  --   not in any way affect which transactions are returned
  --   (see https://github.com/bitcoin/bitcoin/pull/199#issuecomment-1514952)
  Maybe Int ->
  IO SinceBlock
listSinceBlock' client mblockHash mminConf =
  callApi client "listsinceblock" $ tja mblockHash ++ tja mminConf

-- | Returns transactions from the blockchain.
listTransactions ::
  Client ->
  -- | Limits the 'BlockTransaction' returned to those from or to
  --   the given 'Account'.
  Account ->
  -- | Limits the number of 'BlockTransaction' returned.
  Int ->
  -- | Number of most recent transactions to skip.
  Int ->
  IO (Vector SimpleTransaction)
listTransactions client account size from =
  listTransactions' client (Just account) (Just size) (Just from)

-- | Returns transactions from the blockchain.
listTransactions' ::
  Client ->
  -- | Limits the 'BlockTransaction' returned to those from or to
  --   the given 'Account'. If 'Nothing' all accounts are
  --   included in the query.
  Maybe Account ->
  -- | Limits the number of 'BlockTransaction' returned. If
  --   'Nothing' all transactions are returned.
  Maybe Int ->
  -- | Number of most recent transactions to skip.
  Maybe Int ->
  IO (Vector SimpleTransaction)
listTransactions' client maccount mcount mfrom =
  callApi client "listtransactions" $ [tjm "*" maccount] ++ tja mcount ++ tja mfrom

-- | List accounts and their current balance.
listAccounts ::
  Client ->
  -- | Minimum number of confirmations required before payments are
  --   included in the balance.
  Maybe Int ->
  IO (HM.HashMap Account BTC)
listAccounts client mconf =
  callApi client "listaccounts" [tjm 1 mconf]

-- | Import an address
importAddress ::
  Client ->
  -- | Address to import
  Address ->
  -- | Optional account, default ""
  Maybe Account ->
  -- | Optional rescan the blockchain, default true
  Maybe Bool ->
  IO ()
importAddress client addr macct mrescan =
  unNil
    <$> callApi
      client
      "importaddress"
      ([tj addr] ++ tja macct ++ tja mrescan)

-- | Data type for detailed transactions. Rules involving 'trCategory' are
--   indications of the most probable value only when the transaction is
--   obtained from 'listTransactions' or 'listSinceBlock' are their associated
--   methods.
data DetailedTransaction = DetailedTransaction
  { -- | The amount of bitcoins transferred.
    dtAmount :: BTC,
    -- | The fees paid to process the transaction. Is 'Nothing' unless
    --   'trCategory' is 'TCSend' or 'TCReceive'.
    dtFee :: Maybe BTC,
    -- | The number of confirmations of the transaction. Is 'Nothing' unless
    --   'trCategory' is 'TCSend' or 'TCReceive'.
    dtConfirmations :: Maybe Integer,
    -- | The transaction id. Is 'Nothing' unless
    --   'trCategory' is 'TCSend' or 'TCReceive'.
    dtTransactionId :: Maybe TransactionID,
    -- | The list of transaction ids containing the same data as the
    --   original transaction (See ID-malleation bug). Is 'Nothing' unless
    --   'trCategory' is 'TCSend' or 'TCReceive'.
    dtWalletConflicts :: Maybe (Vector TransactionID),
    -- | The block time in seconds since epoch (1 Jan 1970 GMT).
    dtTime :: POSIXTime,
    dtTimeReceived :: Maybe POSIXTime,
    -- | Is 'Nothing' unless a comment is associated with the transaction.
    dtComment :: Maybe Text,
    -- | Is 'Nothing' unless a \"to\" is associated with the transaction.
    dtTo :: Maybe Text,
    -- | The details of the transaction.
    dtDetails :: Vector DetailedTransactionDetails,
    -- | Raw data for the transaction.
    dtHex :: RawTransaction
  }
  deriving (Show, Ord, Eq)

instance FromJSON DetailedTransaction where
  parseJSON (Object o) =
    DetailedTransaction
      <$> o .: "amount"
      <*> o .:? "fee"
      <*> o .: "confirmations"
      <*> o .:? "txid"
      <*> o .:? "walletconflicts"
      <*> (fromInteger <$> o .: "time")
      <*> (fmap fromInteger <$> o .:? "timereceived")
      <*> o .:? "comment"
      <*> o .:? "to"
      <*> o .: "details"
      <*> o .: "hex"
  parseJSON _ = mzero

data DetailedTransactionDetails = DetailedTransactionDetails
  { -- | The account name associated with the transaction. The empty string
    --   is the default account.
    dtdReceivingAccount :: Account,
    -- | The bitcoin address of the transaction.
    dtdAddress :: Address,
    -- | The category of the transaction
    dtdCategory :: TransactionCategory,
    -- | The amount of bitcoins transferred.
    dtdAmount :: BTC
  }
  deriving (Show, Ord, Eq)

instance FromJSON DetailedTransactionDetails where
  parseJSON (Object o) =
    DetailedTransactionDetails <$> o .: "account"
      <*> o .: "address"
      <*> o .: "category"
      <*> o .: "amount"
  parseJSON _ = mzero

getTransaction ::
  Client ->
  TransactionID ->
  IO DetailedTransaction
getTransaction client txid =
  callApi client "gettransaction" [tj txid]

-- | Safely copies wallet.dat to the given destination, which can be either a
--   directory, or a path with filename.
backupWallet ::
  Client ->
  FilePath ->
  IO ()
backupWallet client fp =
  unNil <$> callApi client "backupwallet" [tj fp]

-- | Fills the keypool.
keyPoolRefill :: Client -> IO ()
keyPoolRefill client = unNil <$> callApi client "keypoolrefill" []

-- | Stores the wallet decryption key in memory for the given amount of time.
unlockWallet ::
  Client ->
  -- | The decryption key.
  Text ->
  -- | How long to store the key in memory (in seconds).
  Integer ->
  IO ()
unlockWallet client pass timeout =
  unNil <$> callApi client "walletpassphrase" [tj pass, tj timeout]

-- | Changes the wallet passphrase.
changePassword ::
  Client ->
  -- | The old password.
  Text ->
  -- | The new password.
  Text ->
  IO ()
changePassword client old new =
  unNil <$> callApi client "walletpassphrasechange" [tj old, tj new]

-- | Removes the wallet encryption key from memory, locking the wallet.
--
--   After calling this function, you will need to call 'unlockWallet' again
--   before being able to call methods which require the wallet to be unlocked.
--
--   Note: In future releases, we might introduce an "unlocked" monad, so
--         locking and unlocking is automatic.
lockWallet :: Client -> IO ()
lockWallet client = unNil <$> callApi client "walletlock" []

-- | Encrypts the wallet with the given passphrase.
--
--   WARNING: bitcoind will shut down after calling this method. Don't say I
--            didn't warn you.
encryptWallet :: Client -> Text -> IO ()
encryptWallet client pass = stupidAPI <$> callApi client "encryptwallet" [tj pass]
  where
    stupidAPI :: Text -> ()
    stupidAPI = const ()

-- | Just a handy wrapper to help us get only the "isvalid" field of the JSON.
--   The structure is much too complicated for what it needs to do.
newtype IsValid = IsValid {getValid :: Bool}

instance FromJSON IsValid where
  parseJSON (Object o) = IsValid <$> o .: "isvalid"
  parseJSON _ = mzero

-- | Checks if a given address is a valid one.
isAddressValid :: Client -> Address -> IO Bool
isAddressValid client addr = getValid <$> callApi client "validateaddress" [tj addr]

-- | Possible fee estimation modes
data EstimationMode
  = Economical
  | Conservative
  deriving (Eq)

instance ToJSON EstimationMode where
  toJSON Economical = toJSON ("ECONOMICAL" :: String)
  toJSON Conservative = toJSON ("CONSERVATIVE" :: String)

-- | Estimate the fee per kb to send a transaction
estimateSmartFee :: Client -> Word32 -> Maybe EstimationMode -> IO Double
estimateSmartFee client target mode =
    parse =<< callApi client "estimatesmartfee" (Data.Maybe.catMaybes [ Just $ tj target, tj <$> mode ])
    where
    parse = either (throw . BitcoinResultTypeError . BSL8.pack) pure . parseEither parseResp
    parseResp = withObject "estimatesmartfee response" (.: "feerate")

-- | Return information about the given bitcoin address.
getAddrInfo :: Client -> Address -> IO AddrInfo
getAddrInfo client addr = callApi client "getaddressinfo" [ tj addr ]

-- | Information on a given address.
data AddrInfo = AddrInfo { -- | The address in question.
                                 address :: Address
                               -- | The address' balance.
                               , scriptPubKey  :: ScrPubKey
                               , isScript :: Bool
                               , isWitness :: Bool
                               }
    deriving ( Show, Read, Eq, Ord )

instance FromJSON AddrInfo where
    parseJSON (Object o) =
      AddrInfo
        <$> o .:  "address"
        <*> o .:  "scriptPubKey"
        <*> o .:  "isscript"
        <*> o .:  "iswitness"
    parseJSON _ = mzero

newtype ScrPubKey = ScrPubKey {unScrPubKey :: Text}
  deriving stock (Show, Read, Eq, Ord)
  deriving newtype (FromJSON)

data WalletCfg = WalletCfg
  { walletCfgWalletName :: Text,
    walletCfgDisablePrivateKeys :: Maybe Bool,
    walletCfgBlank :: Maybe Bool,
    walletCfgPassphrase :: Maybe Text,
    walletCfgAvoidReuse :: Maybe Bool,
    walletCfgDescriptors :: Maybe Bool,
    walletCfgLoadOnStartup :: Maybe Bool
  }
  deriving stock
    ( Eq,
      Ord,
      Show
    )

data WalletRes = WalletRes
  { walletResName :: Text,
    walletResWarning :: Text
  }
  deriving stock
    ( Eq,
      Ord,
      Show
    )

instance FromJSON WalletRes where
  parseJSON (Object o) =
    WalletRes
      <$> o .: "name"
      <*> o .: "warning"
  parseJSON _ = mzero

defaultWalletCfg :: WalletCfg
defaultWalletCfg =
  WalletCfg
    { walletCfgWalletName = "defaultwallet",
      walletCfgDisablePrivateKeys = Nothing,
      walletCfgBlank = Nothing,
      walletCfgPassphrase = Nothing,
      walletCfgAvoidReuse = Nothing,
      walletCfgDescriptors = Nothing,
      walletCfgLoadOnStartup = Nothing
    }

defaultAutoLoadWalletCfg :: WalletCfg
defaultAutoLoadWalletCfg =
  defaultWalletCfg
    { walletCfgLoadOnStartup = Just True
    }

createWallet :: Client -> WalletCfg -> IO WalletRes
createWallet client cfg =
  callApi
    client
    "createwallet"
    [ tj $ walletCfgWalletName cfg,
      tj $ walletCfgDisablePrivateKeys cfg,
      tj $ walletCfgBlank cfg,
      tj $ walletCfgPassphrase cfg,
      tj $ walletCfgAvoidReuse cfg,
      tj $ walletCfgDescriptors cfg,
      tj $ walletCfgLoadOnStartup cfg
    ]

loadWallet :: Client -> Text -> Maybe Bool -> IO WalletRes
loadWallet client walletName loadOnStartup =
  callApi
    client
    "loadwallet"
    [ tj walletName,
      tj loadOnStartup
    ]
