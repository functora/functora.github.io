{-# OPTIONS_HADDOCK show-extensions #-}

module Bfx.Data.Env
  ( RawEnv (..),
    Env (..),
    newEnv,
    sysEnv,
  )
where

import Bfx.Data.Web
import Bfx.Import.External
import qualified Data.Aeson as A
import Env
  ( Mod,
    Var,
    header,
    help,
    nonempty,
    parse,
    str,
    var,
  )

data RawEnv = RawEnv
  { rawEnvApiKey :: ApiKey,
    rawEnvPrvKey :: PrvKey
  }
  deriving stock
    ( Eq,
      -- | It's safe to derive 'Show' instance,
      -- because 'ApiKey' and 'PrvKey'
      -- instances are safe.
      Show,
      Generic
    )

instance FromJSON RawEnv where
  parseJSON =
    A.genericParseJSON
      A.defaultOptions
        { A.fieldLabelModifier = A.camelTo2 '_' . drop 6
        }

data Env = Env
  { envNonceGen :: NonceGen,
    envApiKey :: ApiKey,
    envPrvKey :: PrvKey
  }
  deriving stock
    ( Eq,
      Show
    )

newEnv :: (MonadIO m) => RawEnv -> m Env
newEnv raw = do
  nonceGen <- newNonceGen
  pure $
    Env
      { envNonceGen = nonceGen,
        envApiKey = rawEnvApiKey raw,
        envPrvKey = rawEnvPrvKey raw
      }

sysEnv ::
  ( MonadIO m
  ) =>
  m Env
sysEnv = do
  nonceGen <- newNonceGen
  liftIO
    . parse (header "Bfx")
    $ Env nonceGen
      <$> var (str <=< nonempty) "BITFINEX_API_KEY" op
      <*> var (str <=< nonempty) "BITFINEX_PRV_KEY" op
  where
    op :: Mod Var a
    op = help mempty
