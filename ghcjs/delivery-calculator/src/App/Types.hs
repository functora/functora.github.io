{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}

module App.Types
  ( Model (..),
    Action (..),
    St (..),
    newSt,
    Asset (..),
    newAsset,
    verifyAsset,
    newFieldPair,
    newFieldPairId,
    newTotal,
    inspectExchangeRate,
    Screen (..),
    isQrCode,
    unQrCode,
    unShareUri,
    stUri,
    stTeleUri,
    setScreenAction,
    pushActionQueue,
    icon,
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
import qualified Data.Generics as Syb
import qualified Data.Version as Version
import Functora.Cfg
import Functora.Miso.Prelude
import qualified Functora.Miso.Theme as Theme
import Functora.Miso.Types as X hiding
  ( newFieldPair,
    newFieldPairId,
  )
import qualified Functora.Miso.Types as FM
import qualified Functora.Miso.Widgets.Field as Field
import qualified Functora.Miso.Widgets.Icon as Icon
import Functora.Money hiding (Currency, Money, Text)
import qualified Functora.Money as Money
import qualified Functora.Prelude as Prelude
import qualified Functora.Rates as Rates
import qualified Functora.Web as Web
import Lens.Micro ((^..))
import qualified Network.URI as NetUri
import qualified Paths_delivery_calculator as Paths
import qualified Text.Regex as Re
import qualified Text.URI as URI

data Model = Model
  { modelMenu :: OpenedOrClosed,
    modelLinks :: OpenedOrClosed,
    modelLoading :: Bool,
    modelState :: St Unique,
    modelUriViewer :: [FieldPair DynamicField Unique],
    modelDonateViewer :: [FieldPair DynamicField Unique],
    modelProducerQueue :: TChan (InstantOrDelayed (Update Model)),
    modelConsumerQueue :: TChan (InstantOrDelayed (Update Model)),
    modelCurrencies :: NonEmpty CurrencyInfo,
    modelWebOpts :: Web.Opts,
    modelMarket :: MVar Rates.Market
  }
  deriving stock (Eq, Generic)

data Action
  = Noop
  | InitUpdate (Maybe (St Unique))
  | SyncInputs
  | ChanUpdate (Model -> Model)
  | PushUpdate (InstantOrDelayed (Update Model))

data St f = St
  { stAssets :: [Asset f],
    stAssetCurrency :: Currency f,
    stExchangeRate :: Field Rational f,
    stExchangeRateAt :: UTCTime,
    stMerchantCurrency :: Currency f,
    stMerchantTele :: Field Unicode f,
    stMerchantFeePercent :: Field DynamicField f,
    stOnlineOrOffline :: OnlineOrOffline,
    stPreview :: Field Unicode f,
    stScreen :: Screen,
    stEnableTheme :: Bool,
    stTheme :: Theme
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
  fee <- newDynamicField $ DynamicFieldNumber 2
  pre <- newTextField "Delivery Calculator"
  pure
    St
      { stAssets = mempty,
        stAssetCurrency = assetCur,
        stExchangeRate = rate,
        stExchangeRateAt = ct,
        stMerchantCurrency = merchantCur,
        stMerchantTele = tele,
        stMerchantFeePercent = fee & #fieldType .~ FieldTypePercent,
        stOnlineOrOffline = Online,
        stPreview = pre & #fieldType .~ FieldTypeTitle,
        stScreen = Main,
        stEnableTheme = True,
        stTheme = Theme.Matcha
      }

data Asset f = Asset
  { assetFieldPairs :: [FieldPair DynamicField f],
    assetModalState :: OpenedOrClosed,
    assetMustVerify :: Bool
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
  link <-
    newFieldPair "Link"
      $ DynamicFieldText mempty
  photo <-
    fmap
      ( (#fieldPairValue . #fieldOpts . #fieldOptsTruncateLimit .~ Nothing)
          . (#fieldPairValue . #fieldType .~ FieldTypeImage)
      )
      . newFieldPair "Photo"
      $ DynamicFieldText mempty
  price <-
    newFieldPair "Price" $ DynamicFieldNumber 0
  qty <-
    newFieldPair "Quantity" $ DynamicFieldNumber 1
  comment <-
    newFieldPair "Comment" $ DynamicFieldText mempty
  pure
    Asset
      { assetFieldPairs =
          [ required link,
            required photo,
            required price,
            required qty,
            comment
          ],
        assetModalState = Opened,
        assetMustVerify = False
      }
  where
    required :: FieldPair DynamicField Unique -> FieldPair DynamicField Unique
    required = #fieldPairValue . #fieldRequired .~ True

verifyAsset :: Asset Unique -> [View Action]
verifyAsset asset =
  case assetFieldPairs asset of
    (link : photo : price : qty : _)
      | assetMustVerify asset -> do
          let failures =
                intersperse (text " ")
                  $ verifyLink
                    (link ^. #fieldPairValue . #fieldInput . #uniqueValue)
                  <> verifyPhoto
                    (photo ^. #fieldPairValue . #fieldInput . #uniqueValue)
                  <> verifyPrice
                    (price ^. #fieldPairValue . #fieldOutput)
                  <> verifyQty
                    (qty ^. #fieldPairValue . #fieldOutput)
          if null failures
            then mempty
            else [keyed "asset-failure" $ blockquote_ mempty failures]
    _ ->
      mempty

verifyLink :: Unicode -> [View Action]
verifyLink "" = [text "Link is missing!"]
verifyLink txt =
  case Re.matchRegex uriRe . from @Unicode @String $ uriOnlyChars txt of
    Just [uri, _] ->
      if isJust $ Re.matchRegex marketRe uri
        then mempty
        else [text "Link has unsupported marketplace!"]
    _ -> [text "Link should have exactly one URL!"]

verifyPhoto :: Unicode -> [View Action]
verifyPhoto "" = [text "Photo is missing!"]
verifyPhoto txt =
  case Re.matchRegex uriRe str of
    Just [_, _] -> mempty
    _ -> [text "Photo is incorrect!"]
  where
    str = from @Unicode @String $ uriOnlyChars txt

verifyPrice :: DynamicField -> [View Action]
verifyPrice = \case
  DynamicFieldNumber x | x > 0 -> mempty
  _ -> [text "Price must be a positive number!"]

verifyQty :: DynamicField -> [View Action]
verifyQty = \case
  DynamicFieldNumber x | x > 0 -> mempty
  _ -> [text "Quantity must be a positive number!"]

uriOnlyChars :: Unicode -> Unicode
uriOnlyChars =
  omap $ \x ->
    if NetUri.isAllowedInURI x
      then x
      else ' '

newFieldPair ::
  ( MonadIO m
  ) =>
  Unicode ->
  DynamicField ->
  m (FieldPair DynamicField Unique)
newFieldPair key val = do
  res <- FM.newFieldPair key val
  pure
    $ res
    & #fieldPairValue
    . #fieldOpts
    . #fieldOptsQrState
    .~ Nothing
    & #fieldPairValue
    . #fieldOpts
    . #fieldOptsAllowCopy
    .~ False

newFieldPairId ::
  Unicode ->
  DynamicField ->
  FieldPair DynamicField Identity
newFieldPairId key val = do
  FM.newFieldPairId key val
    & #fieldPairValue
    . #fieldOpts
    . #fieldOptsQrState
    .~ Nothing
    & #fieldPairValue
    . #fieldOpts
    . #fieldOptsAllowCopy
    .~ False

newTotal :: St Unique -> [FieldPair DynamicField Identity]
newTotal st =
  if base == 0
    then mempty
    else
      [ newFieldPairId ("Subtotal " <> baseCur)
          . DynamicFieldText
          $ inspectRatioDef base,
        newFieldPairId ("Subtotal " <> quoteCur)
          . DynamicFieldText
          $ inspectRatioDef quote,
        newFieldPairId ("Exchange rate")
          . DynamicFieldText
          $ inspectExchangeRate st,
        FieldPair (newTextFieldId "Fee %")
          $ uniqueToIdentity fee
          & #fieldOpts
          . #fieldOptsQrState
          .~ Nothing
          & #fieldOpts
          . #fieldOptsAllowCopy
          .~ False,
        newFieldPairId ("Total " <> quoteCur)
          . DynamicFieldText
          . inspectRatioDef
          . foldField quote
          $ fee
      ]
  where
    fee = st ^. #stMerchantFeePercent
    rate = st ^. #stExchangeRate . #fieldOutput
    base =
      foldl
        ( \acc fps ->
            if any
              ((== FieldTypeNumber) . (^. #fieldPairValue . #fieldType))
              fps
              then acc + foldl foldFieldPair 1 fps
              else acc
        )
        0
        ( st ^.. #stAssets . each . #assetFieldPairs
        )
    quote =
      rate * base
    baseCur =
      st
        ^. #stAssetCurrency
        . #currencyOutput
        . #currencyInfoCode
        . to Money.inspectCurrencyCode
        . to toUpper
    quoteCur =
      st
        ^. #stMerchantCurrency
        . #currencyOutput
        . #currencyInfoCode
        . to Money.inspectCurrencyCode
        . to toUpper

inspectExchangeRate :: St f -> Unicode
inspectExchangeRate st =
  "1 "
    <> toUpper
      ( Money.inspectCurrencyCode
          $ st
          ^. #stAssetCurrency
          . #currencyOutput
          . #currencyInfoCode
      )
    <> " \8776 "
    <> inspectRatioDef
      ( st ^. #stExchangeRate . #fieldOutput
      )
    <> " "
    <> toUpper
      ( Money.inspectCurrencyCode
          $ st
          ^. #stMerchantCurrency
          . #currencyOutput
          . #currencyInfoCode
      )

foldField :: Rational -> Field DynamicField f -> Rational
foldField acc Field {fieldType = typ, fieldOutput = out} =
  case out of
    DynamicFieldNumber x
      | typ == FieldTypeNumber ->
          acc * x
    DynamicFieldNumber x
      | typ == FieldTypePercent ->
          acc * (1 + (x / 100))
    _ ->
      acc

foldFieldPair :: Rational -> FieldPair DynamicField f -> Rational
foldFieldPair acc =
  foldField acc . fieldPairValue

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
  qxs <-
    stQuery
      . Syb.everywhere
        ( Syb.mkT $ \x ->
            if x ^. #fieldType /= FieldTypeImage
              then x :: Field DynamicField Identity
              else
                x
                  & #fieldInput
                  .~ mempty
                  & #fieldOutput
                  .~ DynamicFieldText mempty
        )
      . uniqueToIdentity
      $ st
      ^. #modelState
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
  base <-
    URI.mkURI "https://t.me"
  user <-
    URI.mkPathPiece
      . from @Unicode @Text
      $ st
      ^. #modelState
      . #stMerchantTele
      . #fieldOutput
  link <-
    stUri st
  key <-
    URI.mkQueryKey "text"
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
      uSt <-
        identityToUnique
          $ Syb.everywhere (Syb.mkT Field.expandDynamicField) iSt
      pure
        $ Just uSt

baseUri :: Unicode
#ifdef GHCID
baseUri =
  "http://localhost:8080"
#else
baseUri =
  "https://functora.github.io/apps/delivery-calculator/" <> vsn <> "/index.html"
#endif

setScreenPure :: Screen -> Update Model
setScreenPure sc =
  PureUpdate
    $ (& #modelMenu .~ Closed)
    . (& #modelLinks .~ Closed)
    . (& #modelState . #stScreen .~ sc)

setScreenAction :: Screen -> Action
setScreenAction =
  PushUpdate
    . Instant
    . setScreenPure

pushActionQueue ::
  ( MonadIO m
  ) =>
  Model ->
  InstantOrDelayed (Update Model) ->
  m ()
pushActionQueue st =
  liftIO
    . atomically
    . writeTChan (st ^. #modelProducerQueue)

icon :: Icon.Icon -> View action
icon = Icon.icon @Icon.Fa

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

uriRe :: Re.Regex
uriRe =
  Re.mkRegex "((https?|ftp)://[^\\s/$.?#].[^\\s]*)"

marketRe :: Re.Regex
marketRe =
  Re.mkRegex "(tb\\.cn|1688\\.com|dewu\\.com|taobao\\.com|tmall\\.com)"
