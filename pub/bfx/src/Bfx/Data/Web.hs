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
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Prelude

newtype PrvKey = PrvKey
  { unPrvKey :: BS.ByteString
  }
  deriving newtype
    ( Eq,
      Ord,
      IsString
    )

instance From BS.ByteString PrvKey

instance From PrvKey BS.ByteString

instance From (UTF_8 BS.ByteString) PrvKey where
  from =
    via @BS.ByteString

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

instance From (UTF_8 BS.ByteString) ApiKey where
  from =
    via @BS.ByteString

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
      Show,
      Data,
      Generic
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
  deriving stock
    ( Data,
      Generic
    )

instance From ByteString RawResponse

instance From RawResponse ByteString

instance Show RawResponse where
  show x =
    case decodeUtf8' bs of
      Left {} -> "ByteString RawResponse" <> inspect (BS.unpack bs)
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
