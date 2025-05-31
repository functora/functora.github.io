{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
-- | An interface to bitcoind's available network-related RPC calls.
--   The implementation of these functions can be found at
--   <https://github.com/bitcoin/bitcoin/blob/master/src/rpcnet.cpp>.
--
--   If any APIs are missing, patches are always welcome. If you look at the
--   source of this module, you'll see that the interface code is trivial.
module Network.Bitcoin.Net ( Client
                           , getClient
                           , getConnectionCount
                           , PeerInfo(..)
                           , getPeerInfo
                           , AddNodeCommand(..)
                           , addNode
                           , disconnectNode
                           , setNetworkActive
                           ) where

import           Control.Monad
import           Data.Aeson
import           Network.Bitcoin.Internal

-- | Returns the number of connections to other nodes.
getConnectionCount :: Client -> IO Integer
getConnectionCount client = callApi client "getconnectioncount" []

-- | Information about a peer node of the Bitcoin network.
--
--   The documentation for this data structure is incomplete, as I honestly
--   don't know what some of these fields are for. Patches are welcome!
data PeerInfo =
    PeerInfo { -- | The IP:port of this peer, as a string.
               addressName    :: Text
             , services       :: Text
             -- | Relative to the first time we conected with this peer (and in
             -- milliseconds), the last time we sent this peer any data.
             , lastSend       :: Integer
             -- | Relative to the first time we connected with this peer
             --   (and in milliseconds), the last time we sent this peer any
             --   data.
             , lastRecv       :: Integer
             , bytesSent      :: Integer
             , bytesRecv      :: Integer
              -- | How long have we been connected to this peer (in
              --   milliseconds).
             , connectionTime :: Integer
             -- | The version of the Bitcion client the peer is running.
             , peerVersion    :: Integer
             -- | The sub-version of the Bitcoin client the peer is running.
             , peerSubversion :: Text
             , inbound        :: Bool
             , startingHeight :: Integer
             -- | How many times has this peer behaved badly?
             , banScore       :: Integer
             }
    deriving ( Show, Read, Ord, Eq )

instance FromJSON PeerInfo where
    parseJSON (Object o) = PeerInfo <$> o .: "addr"
                                    <*> o .: "services"
                                    <*> o .: "lastsend"
                                    <*> o .: "lastrecv"
                                    <*> o .: "bytessent"
                                    <*> o .: "bytesrecv"
                                    <*> o .: "conntime"
                                    <*> o .: "version"
                                    <*> o .: "subver"
                                    <*> o .: "inbound"
                                    <*> o .: "startingheight"
                                    <*> o .: "banscore"
    parseJSON _ = mzero

-- | Returns data about all connected peer nodes.
getPeerInfo :: Client -> IO [PeerInfo]
getPeerInfo client = callApi client "getpeerinfo" []


data AddNodeCommand = Add | Remove | OneTry
    deriving (Show, Read, Eq)


instance ToJSON AddNodeCommand where
    toJSON Add    = String "add"
    toJSON Remove = String "remove"
    toJSON OneTry = String "onetry"


addNode :: Client -> Text -> AddNodeCommand -> IO ()
addNode client url com = unNil <$> callApi client "addnode" [tj url, tj com]


disconnectNode :: Client -> Maybe Text -> Maybe Int -> IO ()
disconnectNode client url nodeId = unNil <$> callApi client "disconnectnode" [tj url, tj nodeId]

setNetworkActive :: Client -> Bool -> IO Bool
setNetworkActive client state = callApi client "setnetworkactive" [tj state]
