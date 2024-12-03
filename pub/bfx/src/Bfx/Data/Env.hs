{-# OPTIONS_HADDOCK show-extensions #-}

module Bfx.Data.Env
  ( Env (..),
    newEnv,
    sysEnv,
  )
where

import Bfx.Data.Web
import Bfx.Import.External
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

data Env = Env
  { envNonceGen :: NonceGen,
    envApiKey :: ApiKey,
    envPrvKey :: PrvKey
  }
  deriving stock
    ( Eq,
      Show
    )

newEnv :: (MonadIO m) => ApiKey -> PrvKey -> m Env
newEnv apiKey prvKey = do
  nonceGen <- newNonceGen
  pure
    $ Env
      { envNonceGen = nonceGen,
        envApiKey = apiKey,
        envPrvKey = prvKey
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
