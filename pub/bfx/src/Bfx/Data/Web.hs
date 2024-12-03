{-# OPTIONS_HADDOCK show-extensions #-}

module Bfx.Data.Web
  ( PrvKey (..),
    ApiKey (..),
    RequestMethod (..),
    BaseUrl (..),
    RawResponse (..),
    Nonce,
    unNonce,
    NonceGen,
    newNonceGen,
    withNonce,
    parseJsonBs,
    utcTimeToMicros,
  )
where

import Bfx.Import.External
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.ByteString as BS
import qualified Prelude

newtype PrvKey = PrvKey
  { unPrvKey :: Text
  }
  deriving newtype
    ( Eq,
      Ord,
      Read,
      IsString,
      HasCodec,
      HasItemCodec
    )
  deriving stock (Data, Generic)
  deriving (Show) via Redacted PrvKey

newtype ApiKey = ApiKey
  { unApiKey :: Text
  }
  deriving newtype
    ( Eq,
      Ord,
      Read,
      IsString,
      HasCodec,
      HasItemCodec
    )
  deriving stock (Data, Generic)
  deriving (Show) via Redacted ApiKey

data RequestMethod
  = GET
  | POST
  deriving stock
    ( Eq,
      Ord,
      Show,
      Read,
      Data,
      Generic
    )

newtype BaseUrl = BaseUrl
  { unBaseUrl :: Text
  }
  deriving newtype
    ( Eq,
      Ord,
      Show,
      Read,
      IsString
    )

newtype RawResponse = RawResponse
  { unRawResponse :: ByteString
  }
  deriving newtype
    ( Eq,
      Ord,
      Show
    )
  deriving stock
    ( Data,
      Generic
    )

newtype Nonce = Nonce
  { unNonce :: Natural
  }
  deriving newtype
    ( Eq,
      Ord,
      Show
    )
  deriving stock
    ( Data,
      Generic
    )

mkNonce :: (MonadIO m) => m Nonce
mkNonce =
  liftIO
    $ Nonce
    . utcTimeToMicros
    <$> getCurrentTime

newtype NonceGen
  = NonceGen (MVar Nonce)
  deriving stock
    ( Eq
    )

instance Prelude.Show NonceGen where
  show =
    const "NonceGen"

newNonceGen ::
  ( MonadIO m
  ) =>
  m NonceGen
newNonceGen = do
  nonce <- mkNonce
  var <- liftIO $ newMVar nonce
  pure $ NonceGen var

withNonce ::
  ( MonadUnliftIO m
  ) =>
  NonceGen ->
  (Nonce -> m a) ->
  m a
withNonce (NonceGen var) this = do
  nextNonce <- mkNonce
  bracket
    (takeMVar var)
    (putMVar var . max nextNonce)
    (const $ this nextNonce)

utcTimeToMicros :: UTCTime -> Natural
utcTimeToMicros x =
  Prelude.fromInteger
    $ diffTimeToPicoseconds
      ( fromRational
          . toRational
          $ diffUTCTime x epoch
      )
    `div` 1000000

parseJsonBs ::
  forall a.
  ( Typeable a,
    From (UTF_8 BS.ByteString) a
  ) =>
  A.Value ->
  A.Parser a
parseJsonBs =
  A.withText (inspectType @a)
    $ pure
    . via @(UTF_8 BS.ByteString)
