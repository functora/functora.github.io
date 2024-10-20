{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}

module App.Types
  ( Model (..),
    Action (..),
    St (..),
    StDoc (..),
    newStDoc,
    Screen (..),
    isQrCode,
    unQrCode,
    unShareUri,
    stUri,
    setScreenPure,
    setScreenAction,
    vsn,
    usd,
    btc,
    googlePlayLink,
    testGroupLink,
    functoraLink,
    sourceLink,
    apkLink,
    module X,
  )
where

import qualified Data.ByteString.Base64.URL as B64URL
import qualified Data.ByteString.Lazy as BL
import Data.Functor.Barbie
import qualified Data.Version as Version
import qualified Functora.Aes as Aes
import Functora.Cfg
import Functora.Miso.Prelude
import Functora.Miso.Types as X
import Functora.Money hiding (Currency, Money, Text)
import qualified Functora.Prelude as Prelude
import qualified Paths_lightning_verifier as Paths
import qualified Text.URI as URI

data Model = Model
  { modelFav :: OpenedOrClosed,
    modelMenu :: OpenedOrClosed,
    modelLinks :: OpenedOrClosed,
    modelLoading :: Bool,
    modelState :: St Unique,
    modelFavMap :: Map Unicode Fav,
    modelFavName :: Field Unicode Unique,
    modelUriViewer :: [FieldPair DynamicField Unique],
    modelProducerQueue :: TChan (InstantOrDelayed (Model -> JSM Model)),
    modelConsumerQueue :: TChan (InstantOrDelayed (Model -> JSM Model))
  }
  deriving stock (Eq, Generic)

data Action
  = Noop
  | InitUpdate (Maybe Aes.Crypto)
  | SyncInputs
  | ChanUpdate Model
  | PushUpdate (InstantOrDelayed (Model -> JSM Model))

data St f = St
  { stKm :: Aes.Km,
    stIkm :: Field Unicode f,
    stDoc :: StDoc f,
    stPre :: Field DynamicField f,
    stScreen :: Screen,
    stCpt :: Maybe Aes.Crypto
  }
  deriving stock (Generic)

deriving stock instance (Hkt f) => Eq (St f)

deriving stock instance (Hkt f) => Ord (St f)

deriving stock instance (Hkt f) => Show (St f)

deriving stock instance (Hkt f) => Data (St f)

instance FunctorB St

instance TraversableB St

deriving via GenericType (St Identity) instance Binary (St Identity)

data StDoc f = StDoc
  { stDocFieldPairs :: [FieldPair DynamicField f],
    stDocSuccessViewer :: [FieldPair DynamicField f],
    stDocFailureViewer :: [FieldPair DynamicField f],
    stDocLnPreimage :: Field Unicode f,
    stDocLnPreimageViewer :: [FieldPair DynamicField f],
    stDocLnInvoice :: Field Unicode f,
    stDocLnInvoiceViewer :: [FieldPair DynamicField f]
  }
  deriving stock (Generic)

deriving stock instance (Hkt f) => Eq (StDoc f)

deriving stock instance (Hkt f) => Ord (StDoc f)

deriving stock instance (Hkt f) => Show (StDoc f)

deriving stock instance (Hkt f) => Data (StDoc f)

instance FunctorB StDoc

instance TraversableB StDoc

deriving via GenericType (StDoc Identity) instance Binary (StDoc Identity)

newStDoc :: (MonadIO m) => m (StDoc Unique)
newStDoc = do
  r <- newTextField mempty
  ln <- newTextField mempty
  pure
    StDoc
      { stDocFieldPairs = mempty,
        stDocSuccessViewer = mempty,
        stDocFailureViewer = mempty,
        stDocLnPreimage = r,
        stDocLnPreimageViewer = mempty,
        stDocLnInvoice = ln,
        stDocLnInvoiceViewer = mempty
      }

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

unShareUri ::
  ( MonadThrow m,
    MonadIO m
  ) =>
  URI ->
  m (Maybe (St Unique))
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
    Just (vCpt, vKm, vSc, vPre) -> do
      bKm <- either throwString pure . B64URL.decode $ encodeUtf8 vKm
      bCpt <- either throwString pure . B64URL.decode $ encodeUtf8 vCpt
      bSc <- either throwString pure . B64URL.decode $ encodeUtf8 vSc
      bPre <- either throwString pure . B64URL.decode $ encodeUtf8 vPre
      km <- either (throwString . thd3) pure $ decodeBinary bKm
      ikm <- newPasswordField mempty
      cpt <- either (throwString . thd3) pure $ decodeBinary bCpt
      sc <- either (throwString . thd3) pure $ decodeBinary bSc
      iPre <- either (throwString . thd3) pure $ decodeBinary bPre
      pre <- identityToUnique iPre
      doc <- newStDoc
      pure
        $ Just
          St
            { stKm = km,
              stIkm = ikm,
              stDoc = doc,
              stPre = pre,
              stScreen = sc,
              stCpt = Just cpt
            }

stQuery :: (MonadThrow m) => St Identity -> m [URI.QueryParam]
stQuery st = do
  kDoc <- URI.mkQueryKey "d"
  vDoc <-
    (URI.mkQueryValue <=< encodeText)
      . encodeBinary
      $ fromMaybe
        (Aes.encryptHmac aes . encodeBinary . compressViewers $ st ^. #stDoc)
        (st ^. #stCpt)
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
    encodeText :: (MonadThrow m) => BL.ByteString -> m Text
    encodeText =
      either throw pure
        . decodeUtf8Strict
        . B64URL.encode
        . from @BL.ByteString @ByteString

stUri :: (MonadThrow m) => Model -> m URI
stUri st = do
  uri <- mkURI $ from @Unicode @Prelude.Text baseUri
  qxs <- stQuery . uniqueToIdentity $ st ^. #modelState
  pure
    $ uri
      { URI.uriQuery = qxs
      }

compressViewers :: StDoc Identity -> StDoc Identity
compressViewers st =
  st
    & #stDocSuccessViewer
    %~ fmap compress
    & #stDocFailureViewer
    %~ fmap compress
    & #stDocLnPreimageViewer
    %~ fmap compress
    & #stDocLnInvoiceViewer
    %~ fmap compress
  where
    compress ::
      FieldPair DynamicField Identity ->
      FieldPair DynamicField Identity
    compress pair =
      pair
        & #fieldPairKey
        . #fieldInput
        .~ Identity (mempty :: Unicode)
        & #fieldPairKey
        . #fieldOutput
        .~ (mempty :: Unicode)
        & #fieldPairValue
        . #fieldInput
        .~ Identity (mempty :: Unicode)
        & #fieldPairValue
        . #fieldOutput
        .~ DynamicFieldText mempty

baseUri :: Unicode
#ifdef GHCID
baseUri =
  "http://localhost:8080"
#else
baseUri =
  "https://functora.github.io/apps/lightning-verifier/" <> vsn <> "/index.html"
#endif

setScreenPure :: Screen -> Model -> JSM Model
setScreenPure sc =
  pure . (& #modelState . #stScreen .~ sc)

setScreenAction :: Screen -> Action
setScreenAction =
  PushUpdate
    . Instant
    . setScreenPure

vsn :: Unicode
vsn =
  intercalate "."
    . fmap inspect
    $ Version.versionBranch Paths.version

usd :: CurrencyInfo
usd = CurrencyInfo (CurrencyCode "usd") mempty

btc :: CurrencyInfo
btc = CurrencyInfo (CurrencyCode "btc") mempty

googlePlayLink :: URI
googlePlayLink =
  either impureThrow id
    $ mkURI "https://play.google.com/apps/testing/com.functora.lightning_verifier"

testGroupLink :: URI
testGroupLink =
  either impureThrow id
    $ mkURI "https://groups.google.com/g/functora"

functoraLink :: URI
functoraLink =
  either impureThrow id
    $ mkURI "https://functora.github.io/"

sourceLink :: URI
sourceLink =
  either impureThrow id
    $ mkURI
      "https://github.com/functora/functora.github.io/tree/master/ghcjs/lightning-verifier"

apkLink :: URI
apkLink =
  either impureThrow id
    . URI.mkURI
    . from @Unicode @Text
    $ "https://github.com/functora/functora.github.io/releases/download/lightning-verifier-v"
    <> vsn
    <> "/lightning-verifier-v"
    <> vsn
    <> ".apk"
