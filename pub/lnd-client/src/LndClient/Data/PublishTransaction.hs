{-# LANGUAGE FlexibleContexts #-}

module LndClient.Data.PublishTransaction
  ( PublishTransactionRequest (..),
    PublishTransactionResponse (..),
  )
where

import Data.ProtoLens.Message
import LndClient.Import
import qualified Proto.Walletrpc.Walletkit as W
import qualified Proto.Walletrpc.Walletkit_Fields as W

data PublishTransactionRequest = PublishTransactionRequest
  { txHex :: ByteString,
    label :: Text
  }
  deriving stock (Eq, Ord, Show, Read, Data, Generic)

instance ToGrpc PublishTransactionRequest W.Transaction where
  toGrpc x = pure $ msg (txHex x) (label x)
    where
      msg f a =
        defMessage
          & W.txHex
          .~ f
          & W.label
          .~ a

newtype PublishTransactionResponse = PublishTransactionResponse
  { publishError :: Text
  }
  deriving stock (Eq, Ord, Show, Read, Data, Generic)

instance FromGrpc PublishTransactionResponse W.PublishResponse where
  fromGrpc x = Right $ PublishTransactionResponse (x ^. W.publishError)
