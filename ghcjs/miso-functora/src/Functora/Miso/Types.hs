{-# LANGUAGE UndecidableInstances #-}

module Functora.Miso.Types
  ( Typ,
    Hkt,
    Unique (..),
    newUnique,
    newUniqueDuplicator,
    Field (..),
    FieldOpts (..),
    defFieldOpts,
    defTruncateLimit,
    newField,
    newFieldId,
    newRatioField,
    newTextField,
    newTextFieldId,
    newPasswordField,
    newDynamicField,
    newDynamicFieldId,
    newDynamicTitleField,
    DynamicField (..),
    parseDynamicField,
    parseDynamicFieldId,
    inspectDynamicField,
    FieldType (..),
    htmlFieldType,
    userFieldType,
    FieldPair (..),
    newFieldPair,
    newFieldPairId,
    mergeFieldPairs,
    Currency (..),
    newCurrency,
    Money (..),
    newMoney,
    Fav (..),
    InstantOrDelayed (..),
    instantOrDelayedTime,
    instantOrDelayedValue,
    drainTChan,
    qsGet,
    uniqueToIdentity,
    identityToUnique,
    keyed,
    appendAttrs,
    prependViews,
    TopOrBottom (..),
    OnlineOrOffline (..),
    StaticOrDynamic (..),
    LeadingOrTrailing (..),
    EnabledOrDisabled (..),
    FocusedOrBlurred (..),
    OpenedOrClosed (..),
    Update (..),
    addEffect,
    unUpdate,
    themeCssFile,
    noopAll,
    noop,
    BlobOpts (..),
    defBlobOpts,
    module X,
  )
where

import Data.Foldable (foldMap)
import Data.Functor.Barbie
import qualified Data.Generics as Syb
import qualified Data.Map as Map
import Functora.Cfg
import Functora.Miso.Prelude
import Functora.Miso.Theme as X (Theme)
import Functora.Money hiding (Currency, Money)
import qualified Miso.Html.Types as Miso
import qualified Text.Casing as Casing
import qualified Text.URI as URI

type Typ a =
  ( Typeable a,
    Eq a,
    Ord a,
    Show a,
    Data a,
    Binary a
  )

type Hkt f =
  ( Typeable f,
    Foldable1 f,
    Eq (f Unicode),
    Ord (f Unicode),
    Show (f Unicode),
    Data (f Unicode)
  )

data Unique a = Unique
  { uniqueUid :: Uid,
    uniqueValue :: a
  }
  deriving stock (Eq, Ord, Show, Data, Generic)

instance Foldable Unique where
  foldMap f x = f $ uniqueValue x

instance Foldable1 Unique where
  foldMap1 f x = f $ uniqueValue x

newUnique :: (MonadIO m) => a -> m (Unique a)
newUnique x =
  Unique
    <$> newUid
    <*> pure x

newUniqueDuplicator ::
  forall (b :: Type) a m.
  ( Data a,
    Typeable b,
    MonadIO m
  ) =>
  m (a -> a)
newUniqueDuplicator = do
  uid <- newUid
  pure
    $ Syb.everywhere
    $ Syb.mkT
      ( (& #uniqueUid %~ addUid uid) :: Unique b -> Unique b
      )

data Field a f = Field
  { fieldType :: FieldType,
    fieldInput :: f Unicode,
    fieldOutput :: a,
    fieldBlobOpts :: BlobOpts,
    fieldModalState :: OpenedOrClosed,
    fieldFocusState :: FocusedOrBlurred,
    fieldRequired :: Bool,
    fieldOpts :: FieldOpts
  }
  deriving stock (Generic)

deriving stock instance (Typ a, Hkt f) => Eq (Field a f)

deriving stock instance (Typ a, Hkt f) => Ord (Field a f)

deriving stock instance (Typ a, Hkt f) => Show (Field a f)

deriving stock instance (Typ a, Hkt f) => Data (Field a f)

instance FunctorB (Field a)

instance TraversableB (Field a)

deriving via
  GenericType (Field a Identity)
  instance
    (Typ a) => Binary (Field a Identity)

instance (Foldable1 f) => ToQueryField (Field a f) where
  toQueryField = toQueryField . fold1 . fieldInput

instance FromQueryField (Field Rational Identity) where
  fromQueryField k v = do
    out <-
      first (const $ FromQueryInvalidField k v)
        . parseRatio
        $ URI.unRText v
    pure
      $ newFieldId
        FieldTypeNumber
        inspectRatioDef
        out

instance FromQueryField (Field Unicode Identity) where
  fromQueryField k v = do
    out <- castFromQueryField k v
    pure $ newFieldId FieldTypeText id out

data FieldOpts = FieldOpts
  { fieldOptsAllowCopy :: Bool,
    fieldOptsTruncateLimit :: Maybe Int,
    fieldOptsTruncateState :: Maybe OpenedOrClosed,
    fieldOptsQrState :: Maybe OpenedOrClosed
  }
  deriving stock (Eq, Ord, Show, Data, Generic)

deriving via GenericType FieldOpts instance Binary FieldOpts

defFieldOpts :: FieldOpts
defFieldOpts =
  FieldOpts
    { fieldOptsAllowCopy = True,
      fieldOptsTruncateLimit = Just defTruncateLimit,
      fieldOptsTruncateState = Just Closed,
      fieldOptsQrState = Nothing
    }

defTruncateLimit :: Int
defTruncateLimit = 67

newField ::
  (MonadIO m) => FieldType -> a -> (a -> Unicode) -> m (Field a Unique)
newField typ output newInput = do
  input <- newUnique $ newInput output
  pure
    Field
      { fieldType = typ,
        fieldInput = input,
        fieldOutput = output,
        fieldBlobOpts = defBlobOpts,
        fieldModalState = Closed,
        fieldFocusState = Blurred,
        fieldRequired = False,
        fieldOpts = defFieldOpts
      }

newFieldId :: FieldType -> (a -> Unicode) -> a -> Field a Identity
newFieldId typ viewer output =
  Field
    { fieldType = typ,
      fieldInput = Identity $ viewer output,
      fieldOutput = output,
      fieldBlobOpts = defBlobOpts,
      fieldModalState = Closed,
      fieldFocusState = Blurred,
      fieldRequired = False,
      fieldOpts = defFieldOpts
    }

newRatioField :: (MonadIO m) => Rational -> m (Field Rational Unique)
newRatioField output =
  newField
    FieldTypeNumber
    output
    inspectRatioDef

newTextField :: (MonadIO m) => Unicode -> m (Field Unicode Unique)
newTextField output =
  newField FieldTypeText output id

newTextFieldId :: Unicode -> Field Unicode Identity
newTextFieldId output =
  newFieldId FieldTypeText id output

newPasswordField :: (MonadIO m) => Unicode -> m (Field Unicode Unique)
newPasswordField output =
  newField FieldTypePassword output id

newDynamicField :: (MonadIO m) => DynamicField -> m (Field DynamicField Unique)
newDynamicField output =
  newField
    ( case output of
        DynamicFieldText {} -> FieldTypeText
        DynamicFieldNumber {} -> FieldTypeNumber
    )
    output
    inspectDynamicField

newDynamicFieldId :: DynamicField -> Field DynamicField Identity
newDynamicFieldId output =
  newFieldId
    ( case output of
        DynamicFieldText {} -> FieldTypeText
        DynamicFieldNumber {} -> FieldTypeNumber
    )
    inspectDynamicField
    output

newDynamicTitleField ::
  (MonadIO m) => Unicode -> m (Field DynamicField Unique)
newDynamicTitleField =
  fmap (& #fieldType .~ FieldTypeTitle)
    . newDynamicField
    . DynamicFieldText

data DynamicField
  = DynamicFieldText Unicode
  | DynamicFieldNumber Rational
  deriving stock (Eq, Ord, Show, Data, Generic)
  deriving (Binary) via GenericType DynamicField

parseDynamicField :: Field DynamicField Unique -> Maybe DynamicField
parseDynamicField value =
  parseDynamic value $ value ^. #fieldInput . #uniqueValue

parseDynamicFieldId :: Field DynamicField Identity -> Maybe DynamicField
parseDynamicFieldId value =
  parseDynamic value $ value ^. #fieldInput . #runIdentity

parseDynamic :: Field DynamicField f -> Unicode -> Maybe DynamicField
parseDynamic value str =
  case value ^. #fieldType of
    FieldTypeNumber -> DynamicFieldNumber <$> parseRatio str
    FieldTypePercent -> DynamicFieldNumber <$> parseRatio str
    _ -> Just $ DynamicFieldText str

inspectDynamicField :: DynamicField -> Unicode
inspectDynamicField = \case
  DynamicFieldText x -> x
  DynamicFieldNumber x -> inspectRatioDef x

data FieldType
  = -- Rational
    FieldTypeNumber
  | FieldTypePercent
  | -- Textual
    FieldTypeText
  | FieldTypeTitle
  | FieldTypeQrCode
  | FieldTypeHtml
  | FieldTypePassword
  | -- Binary
    FieldTypeImage
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded, Data, Generic)
  deriving (Binary) via GenericType FieldType

htmlFieldType :: FieldType -> Unicode
htmlFieldType = \case
  FieldTypeNumber -> "number"
  FieldTypePercent -> "number"
  FieldTypeText -> "text"
  FieldTypeTitle -> "text"
  FieldTypeQrCode -> "text"
  FieldTypeHtml -> "text"
  FieldTypePassword -> "password"
  FieldTypeImage -> "text"

userFieldType :: FieldType -> Unicode
userFieldType = \case
  FieldTypeNumber -> "Number"
  FieldTypePercent -> "Percent"
  FieldTypeText -> "Text"
  FieldTypeTitle -> "Title"
  FieldTypeQrCode -> "QR code"
  FieldTypeHtml -> "HTML"
  FieldTypePassword -> "Password"
  FieldTypeImage -> "Image"

data FieldPair a f = FieldPair
  { fieldPairKey :: Field a f,
    fieldPairValue :: Field a f
  }
  deriving stock (Generic)

deriving stock instance (Typ a, Hkt f) => Eq (FieldPair a f)

deriving stock instance (Typ a, Hkt f) => Ord (FieldPair a f)

deriving stock instance (Typ a, Hkt f) => Show (FieldPair a f)

deriving stock instance (Typ a, Hkt f) => Data (FieldPair a f)

instance FunctorB (FieldPair a)

instance TraversableB (FieldPair a)

deriving via
  GenericType (FieldPair a Identity)
  instance
    (Typ a) => Binary (FieldPair a Identity)

newFieldPair ::
  (MonadIO m) => Unicode -> DynamicField -> m (FieldPair DynamicField Unique)
newFieldPair key val =
  FieldPair
    <$> newDynamicField (DynamicFieldText key)
    <*> newDynamicField val

newFieldPairId :: Unicode -> DynamicField -> FieldPair DynamicField Identity
newFieldPairId key val =
  FieldPair
    (newDynamicFieldId $ DynamicFieldText key)
    (newDynamicFieldId val)

mergeFieldPairs ::
  ( Foldable1 f
  ) =>
  [FieldPair t f] ->
  [FieldPair t f] ->
  [FieldPair t f]
mergeFieldPairs next prev =
  if ( (length next == length prev)
        && (all (== (mempty, mempty)) prevInputs)
     )
    || (nextInputs == prevInputs)
    then fmap (uncurry merge) $ zip next prev
    else next
  where
    nextInputs :: [(Unicode, Unicode)]
    nextInputs = fmap inputs next
    prevInputs :: [(Unicode, Unicode)]
    prevInputs = fmap inputs prev
    merge :: FieldPair t f -> FieldPair t f -> FieldPair t f
    merge new old =
      old
        & #fieldPairKey
        . #fieldInput
        .~ (new ^. #fieldPairKey . #fieldInput)
        & #fieldPairKey
        . #fieldOutput
        .~ (new ^. #fieldPairKey . #fieldOutput)
        & #fieldPairValue
        . #fieldInput
        .~ (new ^. #fieldPairValue . #fieldInput)
        & #fieldPairValue
        . #fieldOutput
        .~ (new ^. #fieldPairValue . #fieldOutput)
    inputs :: (Foldable1 f) => FieldPair t f -> (Unicode, Unicode)
    inputs x =
      ( fold1 $ x ^. #fieldPairKey . #fieldInput,
        fold1 $ x ^. #fieldPairValue . #fieldInput
      )

data Currency f = Currency
  { currencyInput :: Field Unicode f,
    currencyOutput :: CurrencyInfo,
    currencyModalState :: OpenedOrClosed
  }
  deriving stock (Generic)

deriving stock instance (Hkt f) => Eq (Currency f)

deriving stock instance (Hkt f) => Ord (Currency f)

deriving stock instance (Hkt f) => Show (Currency f)

deriving stock instance (Hkt f) => Data (Currency f)

instance FunctorB Currency

instance TraversableB Currency

deriving via GenericType (Currency Identity) instance Binary (Currency Identity)

instance ToQueryField (Currency f) where
  toQueryField =
    toQueryField
      . (^. #currencyOutput . #currencyInfoCode . #unCurrencyCode)

instance FromQueryField (Currency Identity) where
  fromQueryField _ v =
    pure
      Currency
        { currencyInput = newTextFieldId mempty,
          currencyOutput =
            CurrencyInfo
              { currencyInfoText = mempty,
                currencyInfoCode =
                  CurrencyCode
                    . from @Text @Unicode
                    $ URI.unRText v
              },
          currencyModalState = Closed
        }

newCurrency :: (MonadIO m) => CurrencyInfo -> m (Currency Unique)
newCurrency cur =
  Currency
    <$> newTextField mempty
    <*> pure cur
    <*> pure Closed

data Money f = Money
  { moneyAmount :: Field Rational f,
    moneyCurrency :: Currency f
  }
  deriving stock (Generic)

deriving stock instance (Hkt f) => Eq (Money f)

deriving stock instance (Hkt f) => Ord (Money f)

deriving stock instance (Hkt f) => Show (Money f)

deriving stock instance (Hkt f) => Data (Money f)

instance FunctorB Money

instance TraversableB Money

deriving via GenericType (Money Identity) instance Binary (Money Identity)

newMoney :: (MonadIO m) => Rational -> CurrencyInfo -> m (Money Unique)
newMoney amt cur =
  Money
    <$> newRatioField amt
    <*> newCurrency cur

data Fav = Fav
  { favUri :: URI,
    favCreatedAt :: UTCTime
  }
  deriving stock (Eq, Ord, Show, Data, Generic)
  deriving (ToJSON, FromJSON) via GenericType Fav

--
-- TODO : probably don't need debounce in most cases,
-- maybe just eliminate this wrapper type for reduncancy.
--
data InstantOrDelayed a
  = Instant a
  | Delayed Natural a
  deriving stock (Eq, Ord, Show, Data, Generic)

instantOrDelayedTime :: InstantOrDelayed a -> Natural
instantOrDelayedTime = \case
  Instant {} -> 0
  Delayed delay _ -> delay

instantOrDelayedValue :: InstantOrDelayed a -> a
instantOrDelayedValue = \case
  Instant x -> x
  Delayed _ x -> x

drainTChan :: (MonadIO m) => TChan (InstantOrDelayed a) -> m [a]
drainTChan chan = do
  item <- liftIO . atomically $ readTChan chan
  liftIO
    . fmap ((instantOrDelayedValue item :) . reverse)
    . drainInto []
    $ instantOrDelayedTime item
  where
    drainInto acc delay = do
      item <- atomically $ tryReadTChan chan
      case item of
        Nothing | delay == 0 -> pure acc
        Nothing -> do
          sleepMilliSeconds $ from @Natural @Integer delay
          drainInto acc 0
        Just next ->
          drainInto (instantOrDelayedValue next : acc)
            . max delay
            $ instantOrDelayedTime next

qsGet :: URI.RText 'URI.QueryKey -> [URI.QueryParam] -> Maybe Text
qsGet key =
  asumMap $ \case
    URI.QueryParam k x | k == key -> Just $ URI.unRText x
    _ -> Nothing

uniqueToIdentity :: (FunctorB f) => f Unique -> f Identity
uniqueToIdentity =
  bmap $ Identity . uniqueValue

identityToUnique :: (TraversableB f, MonadIO m) => f Identity -> m (f Unique)
identityToUnique =
  btraverse $ newUnique . runIdentity

keyed :: Unicode -> View action -> View action
keyed key = \case
  Node x0 x1 Nothing x2 x3 -> Node x0 x1 (Just $ Miso.Key key) x2 x3
  x -> x

appendAttrs :: [Attribute action] -> View action -> View action
appendAttrs attrs = \case
  Node x0 x1 x2 x3 x4 -> Node x0 x1 x2 (x3 <> attrs) x4
  x -> x

prependViews :: [View action] -> View action -> View action
prependViews xs = \case
  Node a b c d e -> Node a b c d $ xs <> e
  x -> x

data TopOrBottom
  = Top
  | Bottom
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded, Data, Generic)
  deriving (Binary) via GenericType TopOrBottom
  deriving (ToQueryField, FromQueryField) via GenericEnum TopOrBottom

data OnlineOrOffline
  = Online
  | Offline
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded, Data, Generic)
  deriving (Binary) via GenericType OnlineOrOffline
  deriving (ToQueryField, FromQueryField) via GenericEnum OnlineOrOffline

data StaticOrDynamic
  = Static
  | Dynamic
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded, Data, Generic)
  deriving (Binary) via GenericType StaticOrDynamic
  deriving (ToQueryField, FromQueryField) via GenericEnum StaticOrDynamic

data LeadingOrTrailing
  = Leading
  | Trailing
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded, Data, Generic)
  deriving (Binary) via GenericType LeadingOrTrailing
  deriving (ToQueryField, FromQueryField) via GenericEnum LeadingOrTrailing

data EnabledOrDisabled
  = Enabled
  | Disabled
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded, Data, Generic)
  deriving (Binary) via GenericType EnabledOrDisabled
  deriving (ToQueryField, FromQueryField) via GenericEnum EnabledOrDisabled

data FocusedOrBlurred
  = Focused
  | Blurred
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded, Data, Generic)
  deriving (Binary) via GenericType FocusedOrBlurred
  deriving (ToQueryField, FromQueryField) via GenericEnum FocusedOrBlurred

data OpenedOrClosed
  = Opened
  | Closed
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded, Data, Generic)
  deriving (Binary) via GenericType OpenedOrClosed
  deriving (ToQueryField, FromQueryField) via GenericEnum OpenedOrClosed

data Update model
  = PureUpdate (model -> model)
  | ImpureUpdate (JSM (model -> model))
  | EffectUpdate (JSM ())
  | PureAndImpureUpdate (model -> model) (JSM (model -> model))
  | PureAndEffectUpdate (model -> model) (JSM ())
  deriving stock (Generic)

addEffect :: JSM () -> Update model -> Update model
addEffect eff = \case
  PureUpdate f -> PureAndEffectUpdate f eff
  ImpureUpdate g -> ImpureUpdate $ do
    res <- g
    eff
    pure res
  EffectUpdate e -> EffectUpdate $ e >> eff
  PureAndImpureUpdate f g -> PureAndImpureUpdate f $ do
    res <- g
    eff
    pure res
  PureAndEffectUpdate f e -> PureAndEffectUpdate f $ e >> eff

unUpdate :: Update model -> JSM (model -> model)
unUpdate = \case
  PureUpdate f -> pure f
  ImpureUpdate g -> g >>= pure
  EffectUpdate e -> e >> pure id
  PureAndImpureUpdate f g -> g >>= pure . (f .)
  PureAndEffectUpdate f e -> e >> pure f

themeCssFile :: Theme -> Unicode
themeCssFile =
  (<> ".min.css")
    . from @String @Unicode
    . Casing.kebab
    . inspect @String

noopAll :: (Update model -> action) -> [Attribute action]
noopAll action =
  fmap (noop action)
    $ Map.keys defaultEvents

noop :: (Update model -> action) -> Unicode -> Attribute action
noop action event =
  onWithOptions
    defaultOptions
      { preventDefault = True,
        stopPropagation = True
      }
    event
    emptyDecoder
    . const
    . action
    . EffectUpdate
    $ pure ()

data BlobOpts = BlobOpts
  { blobOptsOpfsDir :: Maybe Unicode,
    blobOptsOpfsFile :: Maybe Unicode,
    blobOptsMaxSizeKb :: Maybe Int
  }
  deriving stock (Eq, Ord, Show, Data, Generic)
  deriving (Binary, ToJSON, FromJSON) via GenericType BlobOpts

defBlobOpts :: BlobOpts
defBlobOpts =
  BlobOpts
    { blobOptsOpfsDir = Nothing,
      blobOptsOpfsFile = Nothing,
      blobOptsMaxSizeKb = Nothing
    }
