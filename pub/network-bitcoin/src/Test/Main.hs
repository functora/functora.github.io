{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE MultiParamTypeClasses #-}


module Main where


import           Network.Bitcoin.BtcEnv
import           Control.Monad            (void)
import           Data.Either              (isRight)
import           Data.Text                (Text)
import           Data.Vector              (empty)
import qualified Data.Vector              as V
import           Network.Bitcoin
import           Network.Bitcoin.Wallet
import           Network.Bitcoin.Internal (callApi, tj)
import qualified Network.Bitcoin.Mining   as M
import           Test.QuickCheck
import           Test.QuickCheck.Monadic
import           Test.Tasty               (TestName, TestTree, defaultMain,
                                           testGroup)
import           Test.Tasty.QuickCheck


main :: IO ()
main = defaultMain . testGroup "network-bitcoin tests" $
    [ canCreateWalletAndGetBalance
    , canGenerateBlocks
    , canListUnspent
    , canGetBlock
    , canGetOutputInfo
    , canGetRawTransaction
    , canGetAddress
    , canSendPayment
    , canGetAddrInfo
    ]


client :: IO Client
client = getClient "http://127.0.0.1:18443" "developer" "developer"

instance BtcEnv IO BtcFailure where
  getBtcCfg =
    pure BtcCfg
      { btcCfgHost = "http://127.0.0.1:18443" ,
        btcCfgUsername = "developer" ,
        btcCfgPassword = "developer",
        btcCfgAutoLoadWallet = Just defaultWalletCfg
      }
  getBtcClient =
    client
  handleBtcFailure =
    pure


nbTest name = testProperty name . once . monadicIO


assertRight :: (Show a) => Either a b -> b
assertRight = \case
    Left e -> error $ "Got left " <> show e
    Right res -> res


canCreateWalletAndGetBalance :: TestTree
canCreateWalletAndGetBalance = nbTest "getBalance" $ do
    res <- run $ withBtc getBalance id
    assert $ assertRight res >= 0


canGenerateBlocks :: TestTree
canGenerateBlocks = nbTest "generateToAddress" $ do
    void . run $ do
        c          <- client
        rewardAddr <- getNewAddress c Nothing
        M.generateToAddress c 101 rewardAddr Nothing
    assert True


canListUnspent :: TestTree
canListUnspent = nbTest "listUnspent" $ do
    void . run $ do
        c <- client
        listUnspent c Nothing Nothing Data.Vector.empty
    assert True


getTopBlock :: Client -> IO Block
getTopBlock c = getBlockCount c >>= getBlockHash c >>= getBlock c


canGetBlock :: TestTree
canGetBlock = nbTest "getBlockCount / getBlockHash / getBlock" $ do
    run $ client >>= getTopBlock
    assert True


canGetRawTransaction :: TestTree
canGetRawTransaction = nbTest "getRawTransactionInfo" $ do
    run $ do
        c <- client
        b <- getTopBlock c
        getRawTransactionInfo c (subTransactions b V.! 0)
    assert True


canGetOutputInfo :: TestTree
canGetOutputInfo = nbTest "getOutputInfo" $ do
    run $ do
        c <- client
        b <- getTopBlock c
        getOutputInfo c (subTransactions b V.! 0) 0
    assert True


canGetAddress :: TestTree
canGetAddress = nbTest "getNewAddress" $ do
    run $ do
        c <- client
        getNewAddress c Nothing
    assert True


canSendPayment :: TestTree
canSendPayment = nbTest "send payment" $ do
    c   <- run client
    bal <- run $ getBalance c
    amt <- pick . suchThat arbitrary $ \x -> x < bal && x > 0

    (addr, recv) <- run $ do
        addr <- getNewAddress c Nothing
        sendToAddress c addr amt Nothing Nothing
        M.generate c 6 Nothing
        (addr,) <$> listReceivedByAddress c

    assert . V.elem (addr, amt) . fmap f $ recv
  where
    f = (,) <$> recvAddress <*> recvAmount


canGetAddrInfo :: TestTree
canGetAddrInfo = nbTest "send payment" $ do
    c    <- run client
    addr <- run $ getNewAddress c Nothing
    info <- run $ getAddrInfo c addr
    assert $ isWitness info
