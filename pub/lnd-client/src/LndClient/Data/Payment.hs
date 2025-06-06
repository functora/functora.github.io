module LndClient.Data.Payment
  ( Payment (..),
    PaymentStatus (..),
  )
where

import LndClient.Import
import qualified Proto.Lnrpc.Ln1 as LnGRPC
import qualified Proto.Lnrpc.Ln1_Fields as LnGRPC

data Payment = Payment
  { paymentHash :: RHash,
    paymentPreimage :: RPreimage,
    valueMsat :: Msat,
    state :: PaymentStatus
  }
  deriving stock (Eq, Ord, Show, Read, Data, Generic)

data PaymentStatus
  = UNKNOWN
  | IN_FLIGHT
  | SUCCEEDED
  | FAILED
  deriving stock (Eq, Ord, Show, Read, Data, Generic, Enum, Bounded)

instance FromGrpc Payment LnGRPC.Payment where
  fromGrpc x = do
    res <-
      Payment
        <$> fromGrpc (x ^. LnGRPC.paymentHash)
        <*> fromGrpc (x ^. LnGRPC.paymentPreimage)
        <*> tryFromGrpcMSat (x ^. LnGRPC.valueMsat)
        <*> fromGrpc (x ^. LnGRPC.status)
    if (state res == SUCCEEDED)
      && (newRHash (paymentPreimage res) /= paymentHash res)
      then
        Left
          . LndError
          $ "paymentPreimage doesn't match paymentHash, got: "
          <> inspect res
      else Right res

instance FromGrpc PaymentStatus LnGRPC.Payment'PaymentStatus where
  fromGrpc x =
    case x of
      LnGRPC.Payment'UNKNOWN -> Right UNKNOWN
      LnGRPC.Payment'IN_FLIGHT -> Right IN_FLIGHT
      LnGRPC.Payment'SUCCEEDED -> Right SUCCEEDED
      LnGRPC.Payment'FAILED -> Right FAILED
      LnGRPC.Payment'PaymentStatus'Unrecognized v ->
        Left
          . FromGrpcError
          $ "Cannot parse PaymentStatus, value:"
          <> inspect v
