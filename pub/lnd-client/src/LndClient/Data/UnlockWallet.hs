module LndClient.Data.UnlockWallet
  ( UnlockWalletRequest (..),
  )
where

import Data.ProtoLens.Message
import LndClient.Import
import qualified Proto.Walletunlocker as LnGRPC
import qualified Proto.Walletunlocker_Fields as LnGRPC
import Prelude (Show (..))

data UnlockWalletRequest = UnlockWalletRequest
  { walletPassword :: LndWalletPassword,
    recoveryWindow :: Int32
    --
    --  TODO : channel_backups
    --
  }
  deriving stock (Eq, Ord, Read, Data, Generic)

instance Show UnlockWalletRequest where
  show = const "SECRET"

instance ToGrpc UnlockWalletRequest LnGRPC.UnlockWalletRequest where
  toGrpc x =
    msg
      <$> toGrpc (walletPassword x)
      <*> toGrpc (recoveryWindow x)
    where
      msg :: ByteString -> Int32 -> LnGRPC.UnlockWalletRequest
      msg gWalletPassword gRecoveryWindow =
        defMessage
          & LnGRPC.walletPassword
          .~ gWalletPassword
          & LnGRPC.recoveryWindow
          .~ gRecoveryWindow
