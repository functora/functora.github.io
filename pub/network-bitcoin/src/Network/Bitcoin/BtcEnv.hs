{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module Network.Bitcoin.BtcEnv
  ( BtcEnv (..),
    BtcCfg (..),
    BtcFailure (..),
    newBtcClient,
    tryBtcMethod,
  )
where

import Control.Exception (Handler (..), catches)
import Control.Monad (void)
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import GHC.Generics (Generic)
import Network.Bitcoin.Internal
import Network.Bitcoin.Wallet (WalletCfg)
import qualified Network.Bitcoin.Wallet as Wallet
import Network.HTTP.Client (HttpException)

class (MonadIO m) => BtcEnv m e | m -> e where
  getBtcCfg :: m BtcCfg
  getBtcClient :: m Client
  handleBtcFailure :: BtcFailure -> m e
  withBtc :: (Client -> a) -> (a -> IO b) -> m (Either e b)
  withBtc method args = do
    cfg <- getBtcCfg
    client <- getBtcClient
    tryBtcMethod cfg client method args
      >>= either (fmap Left . handleBtcFailure) (pure . Right)
  withBtcT :: (Client -> a) -> (a -> IO b) -> ExceptT e m b
  withBtcT method =
    ExceptT . withBtc method

data BtcCfg = BtcCfg
  { btcCfgHost :: Text,
    btcCfgUsername :: Text,
    btcCfgPassword :: Text,
    btcCfgAutoLoadWallet :: Maybe WalletCfg
  }
  deriving stock
    ( Eq,
      Ord,
      Generic
    )

data BtcFailure
  = BtcFailureRpc BitcoinException
  | BtcFailureHttp HttpException
  deriving stock
    ( Show,
      Generic
    )

newBtcClient :: (MonadIO m) => BtcCfg -> m Client
newBtcClient cfg =
  liftIO $
    getClient
      (T.unpack $ btcCfgHost cfg)
      (T.encodeUtf8 $ btcCfgUsername cfg)
      (T.encodeUtf8 $ btcCfgPassword cfg)

tryBtcMethod ::
  ( MonadIO m
  ) =>
  BtcCfg ->
  Client ->
  (Client -> a) ->
  (a -> IO b) ->
  m (Either BtcFailure b)
tryBtcMethod cfg client method args = do
  res <- tryBtcMethodInternal client method args
  case (res, btcCfgAutoLoadWallet cfg) of
    -- https://github.com/bitcoin/bitcoin/blob/48174c0f287b19931ca110670610bd03a03eb914/src/rpc/protocol.h#L80
    (Left (BtcFailureRpc (BitcoinApiError (-18) _)), Just wallet) -> do
      let name = Wallet.walletCfgWalletName wallet
      let load = Wallet.walletCfgLoadOnStartup wallet
      void $ tryBtcMethodInternal client Wallet.createWallet ($ wallet)
      void $ tryBtcMethodInternal client Wallet.loadWallet (\f -> f name load)
      tryBtcMethodInternal client method args
    _ ->
      pure res

tryBtcMethodInternal ::
  ( MonadIO m
  ) =>
  Client ->
  (Client -> a) ->
  (a -> IO b) ->
  m (Either BtcFailure b)
tryBtcMethodInternal client method args =
  liftIO $
    (Right <$> args (method client))
      `catches` [ Handler
                    ( \(e :: BitcoinException) ->
                        pure . Left $ BtcFailureRpc e
                    ),
                  Handler
                    ( \(e :: HttpException) ->
                        pure . Left $ BtcFailureHttp e
                    )
                ]
