{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module BitfinexClient.Data.Web
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
    epoch,
    parseJsonBs,
    utcTimeToMicros,
  )
where

import BitfinexClient.Import.External
import BitfinexClient.Util
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Prelude

newtype PrvKey
  = PrvKey BS.ByteString
  deriving newtype
    ( Eq,
      Ord,
      IsString
    )

instance From BS.ByteString PrvKey

instance From PrvKey BS.ByteString

instance Prelude.Show PrvKey where
  show =
    const "SECRET"

instance FromJSON PrvKey where
  parseJSON =
    parseJsonBs

newtype ApiKey
  = ApiKey BS.ByteString
  deriving newtype
    ( Eq,
      Ord,
      IsString
    )

instance From BS.ByteString ApiKey

instance From ApiKey BS.ByteString

instance Prelude.Show ApiKey where
  show =
    const "SECRET"

instance FromJSON ApiKey where
  parseJSON =
    parseJsonBs

data RequestMethod
  = GET
  | POST
  deriving stock
    ( Eq,
      Ord,
      Show
    )

newtype BaseUrl
  = BaseUrl Text
  deriving newtype
    ( Eq,
      Ord,
      Show,
      IsString
    )

newtype RawResponse
  = RawResponse ByteString
  deriving newtype
    ( Eq,
      Ord
    )

instance From ByteString RawResponse

instance From RawResponse ByteString

instance Show RawResponse where
  show x =
    case decodeUtf8' bs of
      Left {} -> "ByteString RawResponse" <> show (BS.unpack bs)
      Right res -> "Text RawResponse " <> T.unpack res
    where
      bs = BL.toStrict $ from x

newtype Nonce = Nonce
  { unNonce :: Natural
  }
  deriving newtype
    ( Eq,
      Ord,
      Show
    )

mkNonce :: (MonadIO m) => m Nonce
mkNonce =
  liftIO $
    Nonce . utcTimeToMicros <$> getCurrentTime

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
  fromInteger $
    diffTimeToPicoseconds
      ( fromRational
          . toRational
          $ diffUTCTime x epoch
      )
      `div` 1000000

epoch :: UTCTime
epoch =
  posixSecondsToUTCTime 0

parseJsonBs ::
  forall a.
  ( Typeable a,
    From BS.ByteString a,
    'False ~ (BS.ByteString == a)
  ) =>
  A.Value ->
  A.Parser a
parseJsonBs =
  A.withText (showType @a) $
    pure
      . via @BS.ByteString
