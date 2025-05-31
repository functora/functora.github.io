{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# OPTIONS_GHC -Wall #-}

module Network.Bitcoin.BtcMultiEnv
  ( BtcMultiEnv (..),
  )
where

import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Network.Bitcoin.BtcEnv (BtcCfg (..), BtcFailure (..), tryBtcMethod)
import Network.Bitcoin.Internal

class (MonadIO m) => BtcMultiEnv m e owner | m -> e owner where
  getBtcCfg :: owner -> m BtcCfg
  getBtcClient :: owner -> m Client
  handleBtcFailure :: owner -> BtcFailure -> m e
  withBtc :: owner -> (Client -> a) -> (a -> IO b) -> m (Either e b)
  withBtc owner method args = do
    cfg <- getBtcCfg owner
    client <- getBtcClient owner
    tryBtcMethod cfg client method args
      >>= either (fmap Left . handleBtcFailure owner) (pure . Right)
  withBtcT :: owner -> (Client -> a) -> (a -> IO b) -> ExceptT e m b
  withBtcT owner method =
    ExceptT . withBtc owner method
