{-# OPTIONS_HADDOCK show-extensions #-}

module Bfx.Data.CancelOrderMulti
  ( Request (..),
  )
where

import Bfx.Import
import qualified Data.Aeson as A

data Request
  = ByOrderId (Set OrderId)
  | ByOrderClientId (Set (OrderClientId, UTCTime))
  | ByOrderGroupId (Set OrderGroupId)
  | Everything
  deriving stock
    ( Eq,
      Ord,
      Show,
      Generic
    )

instance ToJSON Request where
  toJSON =
    eradicateNull . A.object . \case
      ByOrderId xs ->
        ["id" A..= toJSON xs]
      ByOrderClientId xs ->
        ["cid" A..= toJSON (second utctDay <$> toList xs)]
      ByOrderGroupId xs ->
        ["gid" A..= toJSON xs]
      Everything ->
        ["all" A..= toJSON (1 :: Int)]
