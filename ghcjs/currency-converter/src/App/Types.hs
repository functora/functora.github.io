{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}

module App.Types
  ( Model (..),
    Action (..),
    St (..),
    StConv (..),
    StDoc (..),
    StExt (..),
    Screen (..),
    isQrCode,
    unQrCode,
    pureUpdate,
    unShareUri,
    stUri,
    stExtUri,
    baseUri,
    setScreenPure,
    setScreenAction,
    setExtScreenAction,
    shareLink,
    vsn,
    module X,
  )
where

import qualified Data.ByteString.Base64.URL as B64URL
import qualified Data.ByteString.Lazy as BL
import Data.Functor.Barbie
import qualified Data.Text as T
import qualified Data.Version as Version
import qualified Functora.Aes as Aes
import Functora.Cfg
import Functora.Miso.Prelude
import Functora.Miso.Types as X
import Functora.Money hiding (Currency, Money, Text)
import qualified Functora.Prelude as Prelude
import Functora.Rates
import qualified Functora.Web as Web
import qualified Material.Snackbar as Snackbar
import qualified Paths_app as Paths
import qualified Text.URI as URI

data Model = Model
  { modelFav :: OpenedOrClosed,
    modelMenu :: OpenedOrClosed,
    modelLoading :: Bool,
    modelState :: St Unique,
    modelMarket :: MVar Market,
    modelFavMap :: Map MisoString Fav,
    modelCurrencies :: NonEmpty CurrencyInfo,
    modelSnackbarQueue :: Snackbar.Queue Action,
    modelProducerQueue :: TChan (ChanItem (Model -> Model)),
    modelConsumerQueue :: TChan (ChanItem (Model -> Model)),
    modelOnlineAt :: UTCTime,
    modelWebOpts :: Web.Opts
  }
  deriving stock (Eq, Generic)

data Action
  = Noop
  | InitUpdate (Maybe (StExt Unique))
  | TimeUpdate
  | SyncInputs
  | ChanUpdate Model
  | PushUpdate (JSM (ChanItem (Model -> Model)))

data St f = St
  { stScreen :: Screen,
    stDoc :: StDoc f,
    stIkm :: Field MisoString f,
    stKm :: Aes.Km,
    stPre :: Field DynamicField f,
    stExt :: Maybe (StExt f)
  }
  deriving stock (Generic)

deriving stock instance (Hkt f) => Eq (St f)

deriving stock instance (Hkt f) => Ord (St f)

deriving stock instance (Hkt f) => Show (St f)

deriving stock instance (Hkt f) => Data (St f)

instance FunctorB St

instance TraversableB St

deriving via GenericType (St Identity) instance Binary (St Identity)

data StExt f = StExt
  { stExtKm :: Aes.Km,
    stExtIkm :: Field MisoString f,
    stExtDoc :: Aes.Crypto,
    stExtPre :: Field DynamicField f,
    stExtScreen :: Screen
  }
  deriving stock (Generic)

deriving stock instance (Hkt f) => Eq (StExt f)

deriving stock instance (Hkt f) => Ord (StExt f)

deriving stock instance (Hkt f) => Show (StExt f)

deriving stock instance (Hkt f) => Data (StExt f)

instance FunctorB StExt

instance TraversableB StExt

deriving via
  GenericType (StExt Identity)
  instance
    Binary (StExt Identity)

data StConv f = StConv
  { stConvTopMoney :: Money f,
    stConvBottomMoney :: Money f,
    stConvTopOrBottom :: TopOrBottom,
    stConvCreatedAt :: UTCTime
  }
  deriving stock (Generic)

deriving stock instance (Hkt f) => Eq (StConv f)

deriving stock instance (Hkt f) => Ord (StConv f)

deriving stock instance (Hkt f) => Show (StConv f)

deriving stock instance (Hkt f) => Data (StConv f)

instance FunctorB StConv

instance TraversableB StConv

deriving via GenericType (StConv Identity) instance Binary (StConv Identity)

data StDoc f = StDoc
  { stDocConv :: StConv f,
    stDocPreFavName :: Field MisoString f,
    stDocFieldPairs :: [FieldPair DynamicField f],
    stDocOnlineOrOffline :: OnlineOrOffline
  }
  deriving stock (Generic)

deriving stock instance (Hkt f) => Eq (StDoc f)

deriving stock instance (Hkt f) => Ord (StDoc f)

deriving stock instance (Hkt f) => Show (StDoc f)

deriving stock instance (Hkt f) => Data (StDoc f)

instance FunctorB StDoc

instance TraversableB StDoc

deriving via GenericType (StDoc Identity) instance Binary (StDoc Identity)

data Screen
  = Converter
  | QrCode Screen
  deriving stock (Eq, Ord, Show, Data, Generic)
  deriving (Binary) via GenericType Screen

isQrCode :: Screen -> Bool
isQrCode = \case
  QrCode {} -> True
  _ -> False

unQrCode :: Screen -> Screen
unQrCode = \case
  QrCode sc -> unQrCode sc
  sc -> sc

--
-- NOTE : In most cases we don't need JSM.
--
pureUpdate :: Natural -> (Model -> Model) -> Action
pureUpdate delay =
  PushUpdate
    . pure
    . ChanItem delay

unShareUri ::
  ( MonadThrow m,
    MonadIO m
  ) =>
  URI ->
  m (Maybe (StExt Unique))
unShareUri uri = do
  kKm <- URI.mkQueryKey "k"
  kDoc <- URI.mkQueryKey "d"
  kSc <- URI.mkQueryKey "s"
  kPre <- URI.mkQueryKey "p"
  let qs = URI.uriQuery uri
  case (,,,)
    <$> qsGet kDoc qs
    <*> qsGet kKm qs
    <*> qsGet kSc qs
    <*> qsGet kPre qs of
    Nothing -> pure Nothing
    Just (vDoc, vKm, vSc, vPre) -> do
      bKm <- either throwString pure . B64URL.decode $ encodeUtf8 vKm
      bDoc <- either throwString pure . B64URL.decode $ encodeUtf8 vDoc
      bSc <- either throwString pure . B64URL.decode $ encodeUtf8 vSc
      bPre <- either throwString pure . B64URL.decode $ encodeUtf8 vPre
      km <- either (throwString . thd3) pure $ decodeBinary bKm
      ikm <- newPasswordField mempty
      doc <- either (throwString . thd3) pure $ decodeBinary bDoc
      sc <- either (throwString . thd3) pure $ decodeBinary bSc
      iPre <- either (throwString . thd3) pure $ decodeBinary bPre
      pre <- identityToUnique iPre
      pure
        $ Just
          StExt
            { stExtKm = km,
              stExtIkm = ikm,
              stExtDoc = doc,
              stExtPre = pre,
              stExtScreen = sc
            }

stQuery :: (MonadThrow m) => St Identity -> m [URI.QueryParam]
stQuery st = do
  kDoc <- URI.mkQueryKey "d"
  vDoc <-
    (URI.mkQueryValue <=< encodeText)
      . encodeBinary
      . Aes.encryptHmac aes
      $ encodeBinary (st ^. #stDoc)
  kKm <- URI.mkQueryKey "k"
  vKm <-
    (URI.mkQueryValue <=< encodeText)
      . encodeBinary
      . fromEither
      $ fmap (& #kmIkm .~ Ikm mempty) ekm
  kSc <- URI.mkQueryKey "s"
  vSc <-
    (URI.mkQueryValue <=< encodeText)
      $ encodeBinary (st ^. #stScreen)
  kPre <- URI.mkQueryKey "p"
  vPre <-
    (URI.mkQueryValue <=< encodeText)
      $ encodeBinary (st ^. #stPre)
  pure
    [ URI.QueryParam kDoc vDoc,
      URI.QueryParam kKm vKm,
      URI.QueryParam kSc vSc,
      URI.QueryParam kPre vPre
    ]
  where
    aes :: Aes.SomeAesKey
    aes = Aes.drvSomeAesKey @Aes.Word256 $ fromEither ekm
    ekm :: Either Aes.Km Aes.Km
    ekm =
      case st ^. #stIkm . #fieldOutput of
        ikm | ikm == mempty -> Left (st ^. #stKm)
        ikm -> Right $ (st ^. #stKm) & #kmIkm .~ Ikm (encodeUtf8 ikm)
    encodeText :: (MonadThrow m) => BL.ByteString -> m Prelude.Text
    encodeText =
      either throw pure
        . decodeUtf8'
        . B64URL.encode
        . from @BL.ByteString @ByteString

stExtQuery :: (MonadThrow m) => StExt Identity -> m [URI.QueryParam]
stExtQuery st = do
  kDoc <- URI.mkQueryKey "d"
  vDoc <-
    (URI.mkQueryValue <=< encodeText)
      $ encodeBinary (st ^. #stExtDoc)
  kKm <- URI.mkQueryKey "k"
  vKm <-
    (URI.mkQueryValue <=< encodeText)
      $ encodeBinary (st ^. #stExtKm)
  kSc <- URI.mkQueryKey "s"
  vSc <-
    (URI.mkQueryValue <=< encodeText)
      $ encodeBinary (st ^. #stExtScreen)
  kPre <- URI.mkQueryKey "p"
  vPre <-
    (URI.mkQueryValue <=< encodeText)
      $ encodeBinary (st ^. #stExtPre)
  pure
    [ URI.QueryParam kDoc vDoc,
      URI.QueryParam kKm vKm,
      URI.QueryParam kSc vSc,
      URI.QueryParam kPre vPre
    ]
  where
    encodeText :: (MonadThrow m) => BL.ByteString -> m Prelude.Text
    encodeText =
      either throw pure
        . decodeUtf8'
        . B64URL.encode
        . from @BL.ByteString @ByteString

stUri :: (MonadThrow m) => Model -> m URI
stUri Model {modelState = St {stExt = Just ext}} =
  stExtUri ext
stUri st@Model {modelState = St {stExt = Nothing}} = do
  uri <- mkURI $ from @MisoString @Prelude.Text baseUri
  qxs <- stQuery . uniqueToIdentity $ st ^. #modelState
  pure
    $ uri
      { URI.uriQuery = qxs
      }

stExtUri :: (MonadThrow m) => StExt Unique -> m URI
stExtUri ext = do
  uri <- mkURI $ from @MisoString @Prelude.Text baseUri
  qxs <- stExtQuery $ uniqueToIdentity ext
  pure
    $ uri
      { URI.uriQuery = qxs
      }

baseUri :: MisoString
#ifdef GHCID
baseUri =
  "http://localhost:8080"
#else
baseUri =
  "https://functora.github.io/apps/currency-converter/" <> vsn <> "/index.html"
#endif

setScreenPure :: Screen -> Model -> Model
setScreenPure sc =
  (& #modelState . #stScreen .~ sc)
    . (& #modelState . #stExt . _Just . #stExtScreen .~ sc)

setScreenAction :: Screen -> Action
setScreenAction =
  pureUpdate 0 . setScreenPure

setExtScreenAction :: Screen -> Action
setExtScreenAction sc =
  pureUpdate 0 (& #modelState . #stExt . _Just . #stExtScreen .~ sc)

shareLink :: forall a. (From Prelude.Text a) => Screen -> Model -> a
shareLink sc =
  from @Prelude.Text @a
    . either impureThrow URI.render
    . stUri
    . setScreenPure sc

vsn :: MisoString
vsn =
  from @Prelude.Text @MisoString
    . T.intercalate "."
    . fmap Prelude.inspect
    $ Version.versionBranch Paths.version
