{-# LANGUAGE FlexibleContexts #-}

module LndClient.Data.SendCoins
  ( SendCoinsRequest (..),
    SendCoinsResponse (..),
  )
where

import Data.ProtoLens.Message
import LndClient.Import
import qualified Proto.Lightning as LnGRPC
import qualified Proto.Lightning_Fields as LnGRPC

data SendCoinsRequest = SendCoinsRequest
  { addr :: Text,
    amount :: Msat,
    sendAll :: Bool
  }
  deriving stock (Eq, Ord, Show, Read, Data, Generic)

newtype SendCoinsResponse = SendCoinsResponse
  { txid :: Text
  }
  deriving stock (Eq, Ord, Show, Read, Data, Generic)

instance ToGrpc SendCoinsRequest LnGRPC.SendCoinsRequest where
  toGrpc x =
    msg
      <$> toGrpcSat (amount x)
      <*> toGrpc (addr x)
      <*> toGrpc (sendAll x)
    where
      msg gAmt gAddr gSendAll =
        defMessage
          & LnGRPC.amount
          .~ gAmt
          & LnGRPC.addr
          .~ gAddr
          & LnGRPC.sendAll
          .~ gSendAll

instance FromGrpc SendCoinsResponse LnGRPC.SendCoinsResponse where
  fromGrpc x = SendCoinsResponse <$> fromGrpc (x ^. LnGRPC.txid)
