{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}

module App.Types
  ( Model (..),
    Action (..),
    St (..),
    newSt,
    Asset (..),
    newAsset,
    Screen (..),
    isQrCode,
    unQrCode,
    unShareUri,
    stUri,
    stTeleUri,
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
import Functora.Miso.Types as X hiding (Asset (..), newAsset)
import Functora.Money hiding (Currency, Money, Text)
import qualified Functora.Prelude as Prelude
import qualified Functora.Rates as Rates
import qualified Functora.Web as Web
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
    modelConsumerQueue :: TChan (InstantOrDelayed (Model -> JSM Model)),
    modelCurrencies :: NonEmpty CurrencyInfo,
    modelWebOpts :: Web.Opts,
    modelMarket :: MVar Rates.Market
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
    stAssetCurrency :: Currency f,
    stExchangeRate :: Field Rational f,
    stExchangeRateAt :: UTCTime,
    stMerchantCurrency :: Currency f,
    stMerchantTele :: Field Unicode f,
    stMerchantFeePercent :: Field Rational f,
    stOnlineOrOffline :: OnlineOrOffline,
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
  assetCur <- newCurrency cny
  rate <- newRatioField 1
  ct <- getCurrentTime
  merchantCur <- newCurrency rub
  tele <- newTextField "Functora"
  fee <- newRatioField 2
  fav <- newTextField mempty
  pre <- newTextField "Delivery Calculator"
  pure
    St
      { stAssets = mempty,
        stAssetCurrency = assetCur,
        stExchangeRate = rate,
        stExchangeRateAt = ct,
        stMerchantCurrency = merchantCur,
        stMerchantTele = tele,
        stMerchantFeePercent = fee,
        stOnlineOrOffline = Online,
        stFavName = fav,
        stPreview = pre & #fieldType .~ FieldTypeTitle,
        stScreen = Main
      }

data Asset f = Asset
  { assetLink :: Field Unicode f,
    assetPhoto :: Field Unicode f,
    assetPrice :: Field Rational f,
    assetQty :: Field Rational f,
    assetOoc :: OpenedOrClosed
  }
  deriving stock (Generic)

deriving stock instance (Hkt f) => Eq (Asset f)

deriving stock instance (Hkt f) => Ord (Asset f)

deriving stock instance (Hkt f) => Show (Asset f)

deriving stock instance (Hkt f) => Data (Asset f)

instance FunctorB Asset

instance TraversableB Asset

deriving via GenericType (Asset Identity) instance Binary (Asset Identity)

newAsset :: (MonadIO m) => m (Asset Unique)
newAsset = do
  link <- newTextField mempty
  photo <- newTextField mempty
  price <- newRatioField 0
  qty <- newRatioField 1
  pure
    Asset
      { assetLink = link,
        assetPhoto = photo,
        assetPrice = price,
        assetQty = qty,
        assetOoc = Opened
      }

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

stTeleUri :: (MonadThrow m) => Model -> m URI
stTeleUri st = do
  base <- URI.mkURI "https://t.me"
  user <- URI.mkPathPiece $ st ^. #modelState . #stMerchantTele . #fieldOutput
  link <- stUri st
  key <- URI.mkQueryKey "text"
  val <-
    URI.mkQueryValue
      $ "Hello, I have a question about the delivery of the following items: "
      <> URI.render link
  pure
    $ base
      { URI.uriPath = Just (False, [user]),
        URI.uriQuery = [URI.QueryParam key val]
      }

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
  pure
    . (& #modelFav .~ Closed)
    . (& #modelMenu .~ Closed)
    . (& #modelLinks .~ Closed)
    . (& #modelState . #stScreen .~ sc)

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
