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
    pureUpdate,
    unShareUri,
    stUri,
    setScreenPure,
    setScreenAction,
    shareLink,
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
import qualified Data.Text as T
import qualified Data.Version as Version
import qualified Functora.Aes as Aes
import Functora.Cfg
import Functora.Miso.Prelude
import Functora.Miso.Types as X
import Functora.Money hiding (Currency, Money, Text)
import qualified Functora.Prelude as Prelude
import qualified Functora.Rates as Rates
import qualified Functora.Web as Web
import qualified Material.Snackbar as Snackbar
import qualified Paths_app as Paths
import qualified Text.URI as URI

data Model = Model
  { modelFav :: OpenedOrClosed,
    modelMenu :: OpenedOrClosed,
    modelLinks :: OpenedOrClosed,
    modelLoading :: Bool,
    modelState :: St Unique,
    modelMarket :: MVar Rates.Market,
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
  | InitUpdate (Maybe Aes.Crypto)
  | TimeUpdate
  | SyncInputs
  | ChanUpdate Model
  | PushUpdate (JSM (ChanItem (Model -> Model)))

data St f = St
  { stKm :: Aes.Km,
    stIkm :: Field MisoString f,
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
  { stDocTopMoney :: Money f,
    stDocBottomMoney :: Money f,
    stDocTopOrBottom :: TopOrBottom,
    stDocPreFavName :: Field MisoString f,
    stDocFieldPairs :: [FieldPair DynamicField f],
    stDocOnlineOrOffline :: OnlineOrOffline,
    stDocCreatedAt :: UTCTime
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
  ct <- getCurrentTime
  topMoney <- newMoney 1 btc
  bottomMoney <- newMoney 0 usd
  preFavName <- newTextField mempty
  pure
    StDoc
      { stDocTopMoney = topMoney,
        stDocBottomMoney = bottomMoney,
        stDocTopOrBottom = Top,
        stDocPreFavName = preFavName,
        stDocFieldPairs = mempty,
        stDocOnlineOrOffline = Online,
        stDocCreatedAt = ct
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
        (Aes.encryptHmac aes . encodeBinary $ st ^. #stDoc)
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
    encodeText :: (MonadThrow m) => BL.ByteString -> m Prelude.Text
    encodeText =
      either throw pure
        . decodeUtf8'
        . B64URL.encode
        . from @BL.ByteString @ByteString

stUri :: (MonadThrow m) => Model -> m URI
stUri st = do
  uri <- mkURI $ fromMisoString baseUri
  qxs <- stQuery . uniqueToIdentity $ st ^. #modelState
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

setScreenAction :: Screen -> Action
setScreenAction =
  pureUpdate 0 . setScreenPure

shareLink :: forall a. (From Prelude.Text a) => Model -> a
shareLink =
  from @Prelude.Text @a
    . either impureThrow URI.render
    . stUri

vsn :: MisoString
vsn =
  from @Prelude.Text @MisoString
    . T.intercalate "."
    . fmap Prelude.inspect
    $ Version.versionBranch Paths.version

usd :: CurrencyInfo
usd = CurrencyInfo (CurrencyCode "usd") mempty

btc :: CurrencyInfo
btc = CurrencyInfo (CurrencyCode "btc") mempty

googlePlayLink :: Prelude.Text
googlePlayLink = "https://play.google.com/apps/testing/com.functora.currency_converter"

testGroupLink :: Prelude.Text
testGroupLink = "https://groups.google.com/g/currency-converter"

functoraLink :: Prelude.Text
functoraLink = "https://functora.github.io/"

sourceLink :: Prelude.Text
sourceLink =
  "https://github.com/functora/functora.github.io/tree/master/ghcjs/currency-converter"

apkLink :: Prelude.Text
apkLink =
  "https://github.com/functora/functora.github.io/releases/download/currency-converter-v"
    <> fromMisoString vsn
    <> "/currency-converter-v"
    <> fromMisoString vsn
    <> ".apk"
