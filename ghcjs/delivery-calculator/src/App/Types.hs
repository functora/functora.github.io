{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}

module App.Types
  ( Model (..),
    Action (..),
    St (..),
    newSt,
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
    cny,
    rub,
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
import Functora.Cfg
import Functora.Miso.Prelude
import Functora.Miso.Types as X hiding (Asset (..))
import Functora.Money hiding (Currency, Money, Text)
import qualified Functora.Prelude as Prelude
import qualified Paths_delivery_calculator as Paths
import qualified Text.URI as URI

data Model = Model
  { modelFav :: OpenedOrClosed,
    modelMenu :: OpenedOrClosed,
    modelLinks :: OpenedOrClosed,
    modelLoading :: Bool,
    modelState :: St Unique,
    modelFavMap :: Map Unicode Fav,
    modelUriViewer :: [FieldPair DynamicField Unique],
    modelDonateViewer :: [FieldPair DynamicField Unique],
    modelProducerQueue :: TChan (InstantOrDelayed (Model -> JSM Model)),
    modelConsumerQueue :: TChan (InstantOrDelayed (Model -> JSM Model))
  }
  deriving stock (Eq, Generic)

data Action
  = Noop
  | InitUpdate (Maybe (St Unique))
  | SyncInputs
  | ChanUpdate Model
  | PushUpdate (InstantOrDelayed (Model -> JSM Model))

data St f = St
  { stAssets :: [Asset f],
    stPayments :: [Money f],
    stFeePercent :: Field Rational f,
    stDefAssetCurrency :: Currency f,
    stDefPaymentCurrency :: Currency f,
    stFavName :: Field Unicode f,
    stPreview :: Field Unicode f,
    stScreen :: Screen
  }
  deriving stock (Generic)

deriving stock instance (Hkt f) => Eq (St f)

deriving stock instance (Hkt f) => Ord (St f)

deriving stock instance (Hkt f) => Show (St f)

deriving stock instance (Hkt f) => Data (St f)

instance FunctorB St

instance TraversableB St

deriving via GenericType (St Identity) instance Binary (St Identity)

newSt :: (MonadIO m) => m (St Unique)
newSt = do
  fee <- newRatioField 2
  assetCur <- newCurrency cny
  paymentCur <- newCurrency rub
  fav <- newTextField mempty
  pre <- newTextField mempty
  pure
    St
      { stAssets = mempty,
        stPayments = mempty,
        stFeePercent = fee,
        stDefAssetCurrency = assetCur,
        stDefPaymentCurrency = paymentCur,
        stFavName = fav,
        stPreview = pre,
        stScreen = Main
      }

data Asset f = Asset
  { assetLink :: Field URI f,
    assetPhoto :: Field URI f,
    assetPrice :: Money f,
    assetQty :: Field Natural f
  }
  deriving stock (Generic)

deriving stock instance (Hkt f) => Eq (Asset f)

deriving stock instance (Hkt f) => Ord (Asset f)

deriving stock instance (Hkt f) => Show (Asset f)

deriving stock instance (Hkt f) => Data (Asset f)

instance FunctorB Asset

instance TraversableB Asset

deriving via GenericType (Asset Identity) instance Binary (Asset Identity)

data Screen
  = Main
  | Donate
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

stUri :: (MonadThrow m) => Model -> m URI
stUri st = do
  uri <- mkURI $ from @Unicode @Prelude.Text baseUri
  qxs <- stQuery . uniqueToIdentity $ st ^. #modelState
  pure
    $ uri
      { URI.uriQuery = qxs
      }

stQuery :: (MonadThrow m) => St Identity -> m [URI.QueryParam]
stQuery st = do
  kSt <- URI.mkQueryKey "d"
  vSt <- URI.mkQueryValue <=< encode $ encodeBinary st
  pure [URI.QueryParam kSt vSt]
  where
    encode :: (MonadThrow m) => BL.ByteString -> m Text
    encode =
      either throw pure
        . decodeUtf8Strict
        . B64URL.encode
        . from @BL.ByteString @ByteString

unShareUri ::
  ( MonadIO m,
    MonadThrow m
  ) =>
  URI ->
  m (Maybe (St Unique))
unShareUri uri = do
  kSt <- URI.mkQueryKey "d"
  case qsGet kSt $ URI.uriQuery uri of
    Nothing -> pure Nothing
    Just tSt -> do
      bSt <- either throwString pure . B64URL.decode $ encodeUtf8 tSt
      iSt <- either (throwString . thd3) pure $ decodeBinary bSt
      uSt <- identityToUnique iSt
      pure $ Just uSt

baseUri :: Unicode
#ifdef GHCID
baseUri =
  "http://localhost:8080"
#else
baseUri =
  "https://functora.github.io/apps/delivery-calculator/" <> vsn <> "/index.html"
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

cny :: CurrencyInfo
cny = CurrencyInfo (CurrencyCode "cny") mempty

rub :: CurrencyInfo
rub = CurrencyInfo (CurrencyCode "rub") mempty

googlePlayLink :: URI
googlePlayLink =
  either impureThrow id
    $ mkURI "https://play.google.com/apps/testing/com.functora.delivery_calculator"

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
      "https://github.com/functora/functora.github.io/tree/master/ghcjs/delivery-calculator"

apkLink :: URI
apkLink =
  either impureThrow id
    . URI.mkURI
    . from @Unicode @Text
    $ "https://github.com/functora/functora.github.io/releases/download/delivery-calculator-v"
    <> vsn
    <> "/delivery-calculator-v"
    <> vsn
    <> ".apk"
