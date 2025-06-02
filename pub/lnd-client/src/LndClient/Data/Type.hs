{-# LANGUAGE TemplateHaskell #-}

module LndClient.Data.Type
  ( LndError (..),
    LoggingStrategy (..),
    LoggingMeta (..),
    LnInitiator (..),
    logDefault,
    logDebug,
  )
where

import qualified Data.Set as Set
import LndClient.Import.External

data LndError
  = ToGrpcError Text
  | FromGrpcError Text
  | LndGrpcError Text
  | LndGrpcException Text
  | LndWalletLocked
  | LndWalletNotExists
  | GrpcUnexpectedResult Text
  | GrpcEmptyResult
  | LndError Text
  | LndEnvError Text
  | TChanTimeout Text
  | NetworkException Text
  | LndIOException Text
  deriving stock (Eq, Ord, Show, Read, Data, Generic)

data LoggingMeta
  = LndHost
  | LndPort
  | LndMethod
  | LndRequest
  | LndRequestGrpc
  | LndResponse
  | LndResponseGrpc
  | LndResponseSub
  | LndResponseGrpcSub
  | LndElapsedSeconds
  | LndElapsedSecondsSub
  | LndMethodCompose
  | LndTestReceiveInvoice
  deriving stock
    ( Eq,
      Ord,
      Show,
      Read,
      Data,
      Generic,
      Enum,
      Bounded
    )

instance ToJSON LoggingMeta

instance FromJSON LoggingMeta

data LoggingStrategy = LoggingStrategy
  { loggingStrategySeverity ::
      Severity ->
      Maybe Timespan ->
      Maybe LndError ->
      Severity,
    loggingStrategySecret :: Bool,
    loggingStrategyMeta :: Set LoggingMeta
  }

data LnInitiator
  = LnInitiatorUnknown
  | LnInitiatorLocal
  | LnInitiatorRemote
  | LnInitiatorBoth
  deriving stock (Eq, Ord, Show, Read, Data, Generic, Enum, Bounded)

derivePersistField "LnInitiator"

logDefault :: LoggingStrategy
logDefault =
  LoggingStrategy
    { loggingStrategySeverity = \x _ _ -> x,
      loggingStrategySecret = True,
      loggingStrategyMeta = Set.fromList enumerate
    }

logDebug :: LoggingStrategy
logDebug =
  LoggingStrategy
    { loggingStrategySeverity = \_ _ _ -> DebugS,
      loggingStrategySecret = True,
      loggingStrategyMeta = Set.fromList enumerate
    }

instance Exception LndError
