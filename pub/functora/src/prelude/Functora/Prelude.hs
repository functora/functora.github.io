{-# LANGUAGE CPP #-}

module Functora.Prelude
  ( -- * Reexport
    -- $reexport
    module X,
    LiftTH,

    -- * Show
    -- $show
    inspect,
    inspectType,
    inspectSymbol,
    display,
    prettyByteString,
    prettyLazyByteString,
    RatioFormat (..),
    DecimalPlacesOverflowFormat (..),
    defaultRatioFormat,
    inspectRatio,
    inspectRatioDef,

    -- * Integral
    -- $integral
    safeFromIntegral,

    -- * Lens
    -- $lens
    view,
    (^.),
    Getter',
    mkGetters,
    voidTraversal,
    constTraversal,

    -- * DerivingVia
    -- $derivingVia
    Redacted (..),

    -- * Async
    -- $async
    spawnLink,
    withSpawnLink,

    -- * Time
    -- $time
    sleepMicroSeconds,
    sleepMilliSeconds,
    sleepSeconds,
    sleepMinutes,
    sleepHours,
    epoch,
    getCurrentTime,
    getCurrentPicos,

    -- * Exceptions
    -- $exceptions
    ParseException,
    throwParseException,
    throwString,

    -- * Parsers
    -- $parsers
    parseRatio,
    utf8FromLatin1,

    -- * QQ
    -- $qq
    qq,
    qqUri,

    -- * Crypto
    -- $crypto
    Ikm (..),
    Okm (..),
    SaltKm (..),
    InfoKm (..),
    OkmByteSize (..),
    sha256Hash,
    sha256Hmac,
    sha256Hkdf,

    -- * Uid
    -- $uid
    Uid (..),
    newUid,
    addUid,
    nilUid,
    nullUid,
    htmlUid,

    -- * Misc
    -- $misc
    mergeMap,
    mergeAlt,
    mergeBy,
    asumMap,
    altM,
    altM',
    takeWhileAcc,
    randomList,
    randomListPure,
    randomByteString,
    randomByteStringPure,
    expBackOff,
    expBackOffSecondsAfter,
    secondsToNominalDiffTime,
    nubOrdNE,
    nubOrdByNE,
    nubOrdOnNE,
    enumerateNE,
    nextEnum,
    prevEnum,
    Textual,
    strip,
    Mono,
    dropAround,
    dropWhileEnd,

    -- * DerivingVia
    -- $derivingVia
    GenericEnum (..),
    GenericType (..),
  )
where

import Control.Concurrent.Async as X
  ( Async,
    asyncThreadId,
    waitAnyCancel,
  )
import qualified Control.Concurrent.Async as Async
import Control.Concurrent.STM as X (atomically)
import Control.Concurrent.STM.TChan as X
  ( TChan,
    dupTChan,
    newBroadcastTChan,
    newBroadcastTChanIO,
    newTChan,
    readTChan,
    tryReadTChan,
    writeTChan,
  )
import qualified Control.Concurrent.Thread.Delay as Delay
import Control.Exception.Safe as X (impureThrow, throw)
import qualified Control.Exception.Safe as Safe
import Control.Lens as X ((^?))
import Control.Lens.Combinators as X
  ( ALens',
    ATraversal',
    LensLike',
    at,
    cloneLens,
    cloneTraversal,
    each,
    first1Of,
    ix,
    makePrisms,
    non,
    review,
    to,
    _Just,
    _Left,
    _Nothing,
    _Right,
  )
import Control.Monad.Extra as X
  ( eitherM,
    fromMaybeM,
    maybeM,
  )
import Control.Monad.Trans.Reader as X (mapReaderT)
import Data.Binary as X (Binary)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Base16.Lazy as BL16
import Data.ByteString.Base58 as X
  ( bitcoinAlphabet,
    decodeBase58,
    encodeBase58,
    flickrAlphabet,
    rippleAlphabet,
  )
import qualified Data.ByteString.Lazy as BL
import qualified Data.Char as Char
import qualified Data.Digest.SHA256 as SHA256
import Data.Either.Extra as X (fromEither)
import Data.Fixed as X (Pico)
import Data.Foldable1 as X (Foldable1, fold1, foldMap1)
import Data.Functor.Contravariant as X (contramap)
import Data.Generics as X (Data)
import qualified Data.Generics as Syb
import Data.Generics.Internal.VL.Iso as X
import Data.Generics.Internal.VL.Lens as X hiding
  ( set,
    view,
    (^.),
  )
import Data.Generics.Internal.VL.Prism as X hiding (Market (..))
import Data.Generics.Labels as X
import Data.Generics.Product as X
import Data.Generics.Sum as X
import Data.Generics.Uniplate.Data as X ()
import qualified Data.HMAC as HMAC
import Data.List.Extra as X (enumerate, notNull, nubOrd, nubOrdBy, nubOrdOn)
import qualified Data.Map.Merge.Strict as Map
import Data.Maybe as X (listToMaybe)
import Data.MonoTraversable as X (omap)
import qualified Data.MonoTraversable as Mono
import Data.MonoTraversable.Unprefixed as X (intercalate)
import Data.Ratio as X ((%))
import Data.Scientific as X (Scientific)
import qualified Data.Scientific as Scientific
import qualified Data.Semigroup as Semigroup
import Data.Sequences as X hiding
  ( Textual,
    Utf8 (..),
    catMaybes,
    dropWhile,
    filterM,
    find,
    fromList,
    group,
    groupBy,
    intersperse,
    permutations,
    replicate,
    replicateM,
    reverse,
    sort,
    sortBy,
    sortOn,
    splitAt,
    subsequences,
    takeWhile,
    uncons,
  )
import qualified Data.Sequences as Seq
import Data.Tagged as X (Tagged (..))
import qualified Data.Text as T
import Data.Text.Encoding as X (decodeLatin1)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text.Read as T
import Data.Time.Clock as X
  ( DiffTime,
    NominalDiffTime,
    UTCTime (..),
    addUTCTime,
    diffTimeToPicoseconds,
    diffUTCTime,
    nominalDay,
    secondsToDiffTime,
  )
import qualified Data.Time.Clock as Clock
import Data.Time.Clock.POSIX as X (posixSecondsToUTCTime)
import Data.Tuple.Extra as X (thd3, uncurry3)
import qualified Data.Typeable as Typeable
import Functora.PreludeOrphan as X ()
import Functora.Unicode as X (Unicode)
import GHC.Generics as X (Rep)
import GHC.TypeLits as X (KnownSymbol, Symbol)
import qualified GHC.TypeLits as TypeLits
import Instances.TH.Lift as X ()
import qualified Language.Haskell.TH.Lib as TH
import Language.Haskell.TH.Quote as X (QuasiQuoter (..))
import qualified Language.Haskell.TH.Syntax as TH
import Lens.Micro as X (Getting, Traversal, Traversal', (%~))
import Lens.Micro.Contra as X (Fold, Getter, fromSimpleFold, fromSimpleGetter)
import Lens.Micro.GHC as X ()
import Lens.Micro.TH as X (makeLenses)
import qualified Lens.Micro.TH as TH
  ( generateUpdateableOptics,
    lensRules,
    makeLensesWith,
  )
import Main.Utf8 as X (withUtf8)
import qualified System.Random as Random
import Text.URI as X (URI, mkURI)
import qualified Text.URI.QQ as URI
import Type.Reflection
import Universum as X hiding
  ( Lens,
    Lens',
    Traversal,
    Traversal',
    atomically,
    bracket,
    break,
    catch,
    catchAny,
    decodeUtf8',
    drop,
    filter,
    finally,
    fromInteger,
    fromIntegral,
    fromStrict,
    handleAny,
    inits,
    intercalate,
    isPrefixOf,
    lines,
    on,
    over,
    preview,
    print,
    set,
    show,
    state,
    swap,
    tails,
    take,
    throwM,
    toStrict,
    try,
    tryAny,
    unlines,
    unwords,
    view,
    words,
    (%~),
    (.~),
    (^.),
    (^..),
    (^?),
    _1,
    _2,
    _3,
    _4,
    _5,
  )
import qualified Universum (fromIntegral, show)
import UnliftIO as X
  ( MonadUnliftIO (..),
    UnliftIO (..),
    askRunInIO,
    bracket,
    catch,
    catchAny,
    catchAnyDeep,
    catchDeep,
    catchIO,
    finally,
    handle,
    handleAny,
    handleAnyDeep,
    handleDeep,
    handleIO,
    race,
    toIO,
    try,
    tryAny,
    tryAnyDeep,
    tryDeep,
    tryIO,
    withRunInIO,
    withUnliftIO,
  )
import UnliftIO.MVar as X (modifyMVar)
import Witch.Mini as X
import qualified Prelude
#if defined(__GHCJS__) || defined(ghcjs_HOST_OS) || defined(wasi_HOST_OS)
import qualified Data.JSString as JS
#endif

-- $reexport
-- Reexport

type LiftTH = TH.Lift

-- $show
-- Show

inspect :: forall dst src. (Show src, Data src, Textual dst) => src -> dst
inspect =
  display @dst @src
    . Syb.everywhere (Syb.mkT prettyByteString)
    . Syb.everywhere (Syb.mkT prettyLazyByteString)

inspectType :: forall a b. (Typeable a, Textual b) => b
inspectType =
  Universum.show
    . Typeable.typeRep
    $ Proxy @a

inspectSymbol :: forall a b. (KnownSymbol a, Textual b) => b
inspectSymbol =
  from @String @b
    . TypeLits.symbolVal
    $ Proxy @a

data RatioFormat = RatioFormat
  { ratioFormatDoRounding :: Bool,
    ratioFormatThousandsSeparator :: Unicode,
    ratioFormatDecimalPlacesAfterNonZero :: Maybe Natural,
    ratioFormatDecimalPlacesTotalLimit :: Maybe Natural,
    ratioFormatDecimalPlacesTotalLimitOverflow :: DecimalPlacesOverflowFormat
  }
  deriving stock (Eq, Ord, Show, Read, Data, Generic)

data DecimalPlacesOverflowFormat
  = DecimalPlacesOverflowExponent
  | DecimalPlacesOverflowTruncate
  deriving stock (Eq, Ord, Show, Read, Data, Generic)

defaultRatioFormat :: RatioFormat
defaultRatioFormat =
  RatioFormat
    { ratioFormatDoRounding = True,
      ratioFormatThousandsSeparator = mempty,
      ratioFormatDecimalPlacesAfterNonZero = Just 2,
      ratioFormatDecimalPlacesTotalLimit = Just 8,
      ratioFormatDecimalPlacesTotalLimitOverflow = DecimalPlacesOverflowExponent
    }

inspectRatio ::
  forall a b.
  ( Textual a,
    From b Integer,
    Integral b
  ) =>
  RatioFormat ->
  Ratio b ->
  a
inspectRatio fmt signedRational =
  let (quotient, remainder) = unsignedNumerator `quotRem` unsignedDenominator
      fractionalPart =
        takeWhileAcc
          format
          (0, if quotient == 0 then 0 else 1)
          (go remainder)
      fractionalSize =
        length fractionalPart
   in if ratioFormatDoRounding fmt
        then
          inspectRatio fmt {ratioFormatDoRounding = False}
            . roundRational fractionalSize
            $ signedNumerator
            % signedDenominator
        else case totalDecimalPlacesLimit of
          Just limit
            | fractionalSize
                >= limit
                && ratioFormatDecimalPlacesTotalLimitOverflow fmt
                == DecimalPlacesOverflowExponent ->
                from @String @a
                  . Scientific.formatScientific Scientific.Exponent Nothing
                  . either fst fst
                  . Scientific.fromRationalRepetend Nothing
                  $ signedNumerator
                  % signedDenominator
          _ ->
            from @String @a
              $ (if signedRational < 0 then "-" else mempty)
              <> Prelude.shows
                quotient
                (if null fractionalPart then mempty else "." <> fractionalPart)
  where
    signedNumerator = from @b @Integer $ numerator signedRational
    signedDenominator = from @b @Integer $ denominator signedRational
    unsignedNumerator = abs signedNumerator
    unsignedDenominator = abs signedDenominator
    nonZeroDecimalPlacesLimit =
      (+ 1) <$> ratioFormatDecimalPlacesAfterNonZero fmt
    totalDecimalPlacesLimit =
      unsafeFrom <$> ratioFormatDecimalPlacesTotalLimit fmt
    go 0 = mempty
    go previous =
      let (current, next) = (10 * previous) `quotRem` unsignedDenominator
       in Prelude.shows current (go next)
    format x (prevTotalDecimalPlaces, prevNonZeroDecimalPlaces) =
      case (totalDecimalPlacesLimit, nonZeroDecimalPlacesLimit) of
        (Just totalLimit, Just nonZeroLimit)
          | prevTotalDecimalPlaces
              >= totalLimit
              && prevNonZeroDecimalPlaces
              >= nonZeroLimit ->
              Nothing
        (Just limit, Nothing)
          | prevTotalDecimalPlaces >= limit && prevNonZeroDecimalPlaces > 0 ->
              Nothing
        (_, Just limit)
          | prevNonZeroDecimalPlaces >= limit ->
              Nothing
        (_, _) ->
          Just
            ( prevTotalDecimalPlaces + 1,
              if prevNonZeroDecimalPlaces /= 0 || x /= '0'
                then prevNonZeroDecimalPlaces + 1
                else 0
            )

inspectRatioDef ::
  forall a b.
  ( Textual a,
    From b Integer,
    Integral b
  ) =>
  Ratio b ->
  a
inspectRatioDef =
  inspectRatio defaultRatioFormat

roundRational :: Int -> Rational -> Rational
roundRational decimalPlaces input =
  round (input * from @Integer @Rational mult) % mult
  where
    mult = 10 ^ decimalPlaces

display :: forall dst src. (Show src, Typeable src, Textual dst) => src -> dst
display x
  | Just HRefl <- typeOf x `eqTypeRep` typeRep @dst =
      x
  | Just HRefl <- typeOf x `eqTypeRep` typeRep @String =
      from @String @dst x
  | Just HRefl <- typeOf x `eqTypeRep` typeRep @Text =
      from @Text @dst x
  | Just HRefl <- typeOf x `eqTypeRep` typeRep @TL.Text =
      from @TL.Text @dst x
  | Just HRefl <- typeOf x `eqTypeRep` typeRep @ByteString =
      either (const $ Universum.show x) id $ decodeUtf8Strict x
  | Just HRefl <- typeOf x `eqTypeRep` typeRep @BL.ByteString =
      either (const $ Universum.show x) id $ decodeUtf8Strict x
  | otherwise =
      defDisplay
  where
#if defined(__GHCJS__) || defined(ghcjs_HOST_OS) || defined(wasi_HOST_OS)
    defDisplay
      | Just HRefl <- typeOf x `eqTypeRep` typeRep @JS.JSString =
          from @JS.JSString @dst x
      | otherwise =
          Universum.show x
#else
    defDisplay =
      Universum.show x
#endif

prettyByteString :: ByteString -> ByteString
prettyByteString raw =
  if isRight $ TE.decodeUtf8' raw
    then raw
    else B16.encode raw

prettyLazyByteString :: BL.ByteString -> BL.ByteString
prettyLazyByteString raw =
  if isRight $ TLE.decodeUtf8' raw
    then raw
    else BL16.encode raw

-- $integral
-- Integral

safeFromIntegral ::
  forall src dst.
  ( Integral src,
    Integral dst,
    Bounded dst
  ) =>
  src ->
  Maybe dst
safeFromIntegral x =
  if (intX >= intMin) && (intX <= intMax)
    then Just $ Universum.fromIntegral x
    else Nothing
  where
    intX = toInteger x :: Integer
    intMin = toInteger (minBound :: dst) :: Integer
    intMax = toInteger (maxBound :: dst) :: Integer

-- $lens
-- NOTE : view override is needed because of this:
-- https://github.com/ekmett/lens/issues/798

view :: Getter' s a -> s -> a
view = first1Of
{-# INLINE view #-}

infixl 8 ^.

(^.) :: s -> Getter' s a -> a
(^.) = flip first1Of
{-# INLINE (^.) #-}

type Getter' s a = Getting (Semigroup.First a) s a

mkGetters :: TH.Name -> TH.DecsQ
mkGetters =
  TH.makeLensesWith $ TH.lensRules & TH.generateUpdateableOptics .~ False

voidTraversal :: ATraversal' s a
voidTraversal = const pure

constTraversal :: a -> ATraversal' s a
constTraversal a = \f s -> const s <$> f a

-- $derivingVia
-- Newtypes to simplify deriving via.
-- We have to expose default constructors/accessors
-- to help GHC with figuring out that runtime representation does match.

newtype Redacted a = Redacted
  { unRedacted :: a
  }

instance (Typeable a) => Show (Redacted a) where
  show = const $ "<REDACTED> :: " <> inspectType @a

-- $async
-- Async

spawnLink :: (MonadUnliftIO m) => m a -> m (Async a)
spawnLink x =
  withRunInIO $ \run -> do
    pid <- Async.async $ run x
    Async.link pid
    pure pid

withSpawnLink :: (MonadUnliftIO m) => m a -> (Async a -> m b) -> m b
withSpawnLink action inner =
  withRunInIO $ \run ->
    Async.withAsync
      (run action)
      ( \pid -> do
          Async.link pid
          run $ inner pid
      )

-- $time
-- Time

sleepMicroSeconds :: (MonadIO m) => Integer -> m ()
sleepMicroSeconds = liftIO . Delay.delay

sleepMilliSeconds :: (MonadIO m) => Integer -> m ()
sleepMilliSeconds = sleepMicroSeconds . (* 1_000)

sleepSeconds :: (MonadIO m) => Integer -> m ()
sleepSeconds = sleepMicroSeconds . (* 1_000_000)

sleepMinutes :: (MonadIO m) => Integer -> m ()
sleepMinutes = sleepSeconds . (* 60)

sleepHours :: (MonadIO m) => Integer -> m ()
sleepHours = sleepMinutes . (* 60)

epoch :: UTCTime
epoch = posixSecondsToUTCTime 0

getCurrentTime :: (MonadIO m) => m UTCTime
getCurrentTime = liftIO Clock.getCurrentTime

getCurrentPicos :: (MonadIO m) => m Integer
getCurrentPicos = do
  ct <- getCurrentTime
  pure
    . diffTimeToPicoseconds
    . fromRational
    . toRational
    $ diffUTCTime ct epoch

-- $exceptions
-- Exceptions

data ParseException = ParseException
  { parseExceptionSource :: Unicode,
    parseExceptionSourceType :: SomeTypeRep,
    parseExceptionTargetType :: SomeTypeRep,
    parseExceptionFailure :: Unicode
  }
  deriving stock (Eq, Ord, Show, Data, Generic)

instance Exception ParseException where
  displayException = inspect

throwParseException ::
  forall dst src e m.
  ( Typeable dst,
    Show src,
    Show e,
    Data src,
    Data e,
    MonadThrow m
  ) =>
  src ->
  e ->
  m dst
throwParseException source failure =
  throw
    ParseException
      { parseExceptionSource = inspect source,
        parseExceptionSourceType = SomeTypeRep $ typeOf source,
        parseExceptionTargetType = SomeTypeRep $ typeRep @dst,
        parseExceptionFailure = inspect failure
      }

throwString ::
  forall e m a.
  ( From e String,
    MonadThrow m,
    HasCallStack
  ) =>
  e ->
  m a
throwString =
  Safe.throwString . from @e @String

-- $parsers
-- Parsers

parseRatio ::
  forall str int m.
  ( From str Text,
    From int Integer,
    Integral int,
    Show str,
    Show int,
    Data str,
    Data int,
    MonadThrow m
  ) =>
  str ->
  m (Ratio int)
parseRatio str =
  case T.rational . T.strip $ from @str @Text str of
    Right (rat, "")
      | Just HRefl <- typeRep @int `eqTypeRep` typeRep @Integer ->
          pure rat
    Right (rat, "") -> do
      let lhsRat =
            from @int @Integer (numerator rat)
              % from @int @Integer (denominator rat)
      rhsRat <- parseRational str
      if lhsRat == rhsRat
        then pure rat
        else
          throwParseException str
            $ inspectType @int @Unicode
            <> " numerator or denominator seems to be out of bounds, expected "
            <> inspect rhsRat
            <> " but got "
            <> inspect lhsRat
    Right e ->
      throwParseException str e
    Left e ->
      throwParseException str e

parseRational ::
  forall str m.
  ( From str Text,
    Show str,
    Data str,
    MonadThrow m
  ) =>
  str ->
  m Rational
parseRational str =
  case T.rational . T.strip $ from @str @Text str of
    Right (rat, "") -> pure rat
    Right e -> throwParseException str e
    Left e -> throwParseException str e

utf8FromLatin1 ::
  forall a.
  ( From a ByteString,
    From ByteString a
  ) =>
  a ->
  a
utf8FromLatin1 raw =
  from @ByteString @a
    . encodeUtf8
    . either (const $ decodeLatin1 bs) id
    $ decodeUtf8Strict bs
  where
    bs = from @a @ByteString raw

-- $qq
-- QQ

qq ::
  forall inp out e.
  ( Textual inp,
    Typeable out,
    TH.Lift out,
    Data e,
    Show e
  ) =>
  (inp -> Either e out) ->
  QuasiQuoter
qq parser =
  QuasiQuoter
    { quoteDec = failure "quoteDec",
      quoteType = failure "quoteType",
      quotePat = failure "quotePat",
      quoteExp =
        \x0 -> do
          let fatal e =
                fail
                  $ "QuasiQuoter failed for the input ("
                  <> x0
                  <> ") of the type ("
                  <> inspectType @out
                  <> ") with the failure ("
                  <> inspect e
                  <> ")"
          case parser $ from @String @inp x0 of
            Left e -> fatal e
            Right x -> TH.lift x
    }
  where
    failure :: forall any. Text -> any
    failure msg =
      error
        $ inspectType @out
        <> " "
        <> msg
        <> " is not implemented"

qqUri :: QuasiQuoter
qqUri = URI.uri

-- $crypto
-- Crypto

newtype Ikm = Ikm
  { unIkm :: ByteString
  }
  deriving stock (Eq, Ord, Read, Data, Generic)
  deriving newtype (Binary)
  deriving (Show) via Redacted Ikm

newtype Okm = Okm
  { unOkm :: ByteString
  }
  deriving stock (Eq, Ord, Read, Data, Generic)
  deriving newtype (Binary)
  deriving (Show) via Redacted Okm

newtype SaltKm = SaltKm
  { unSaltKm :: ByteString
  }
  deriving stock (Eq, Ord, Read, Data, Generic)
  deriving newtype (Binary)
  deriving (Show) via Redacted SaltKm

newtype InfoKm = InfoKm
  { unInfoKm :: ByteString
  }
  deriving stock (Eq, Ord, Read, Data, Generic)
  deriving newtype (Binary)
  deriving (Show) via Redacted InfoKm

newtype OkmByteSize = OkmByteSize
  { unOkmByteSize :: Natural
  }
  deriving stock (Eq, Ord, Show, Read, Data, Generic)
  deriving newtype (Binary)

sha256Hash :: forall a b. (From a [Word8], From [Word8] b) => a -> b
sha256Hash =
  from @[Word8] @b
    . SHA256.hash
    . from @a @[Word8]

sha256Hmac ::
  forall a b c.
  ( From a [Word8],
    From b [Word8],
    From [Word8] c
  ) =>
  a ->
  b ->
  c
sha256Hmac prv msg =
  from @[Word8] @c
    $ HMAC.hmac
      HMAC.HashMethod
        { HMAC.digest = sha256Hash,
          HMAC.input_blocksize = 512
        }
      ( from @a @[Word8] prv
      )
      ( from @b @[Word8] msg
      )

sha256Hkdf :: Ikm -> SaltKm -> InfoKm -> OkmByteSize -> Okm
sha256Hkdf ikm salt info okmByteSize =
  let prk = sha256HkdfExtract salt ikm
   in hkdfExpand prk info okmByteSize

sha256HkdfExtract :: SaltKm -> Ikm -> ByteString
sha256HkdfExtract (SaltKm salt) (Ikm ikm) =
  sha256Hmac salt ikm

hkdfExpand :: ByteString -> InfoKm -> OkmByteSize -> Okm
hkdfExpand prk (InfoKm info) okmByteSize =
  Okm $ expand mempty mempty 0
  where
    size :: Int
    size = unsafeFrom @Natural @Int $ unOkmByteSize okmByteSize
    expand :: ByteString -> ByteString -> Int -> ByteString
    expand acc prev idx =
      if idx * 32 >= size
        then BS.take size acc
        else expand (acc <> next) next $ idx + 1
      where
        next =
          sha256Hmac prk
            $ prev
            <> info
            <> BS.singleton (Prelude.fromIntegral idx + 1)

-- $uid
-- Uid

newtype Uid = Uid
  { unUid :: ByteString
  }
  deriving stock (Eq, Ord, Show, Read, Data, Generic)

instance Binary Uid

newUid :: (MonadIO m) => m Uid
newUid = Uid <$> randomByteString 32

addUid :: Uid -> Uid -> Uid
addUid (Uid lhs) (Uid rhs) = Uid . sha256Hash $ lhs <> rhs

nilUid :: Uid
nilUid = Uid $ BS.replicate 32 0

nullUid :: Uid -> Bool
nullUid = (== nilUid)

htmlUid :: Uid -> UTF_8 ByteString
htmlUid =
  Tagged @"UTF-8"
    . ("uid-" <>)
    . encodeBase58 bitcoinAlphabet
    . unUid

-- $misc
-- Misc

mergeMap :: (Ord a) => (b -> b -> b) -> Map a b -> Map a b -> Map a b
mergeMap upd =
  Map.merge
    (Map.mapMaybeMissing $ const pure)
    (Map.mapMaybeMissing $ const pure)
    (Map.zipWithMaybeMatched . const $ \lhs rhs -> pure $ upd lhs rhs)

mergeAlt :: (Alternative t) => (a -> a -> a) -> t a -> t a -> t a
mergeAlt f x y = liftA2 f x y <|> x <|> y

mergeBy :: (Ord a) => (b -> a) -> (b -> b -> b) -> [b] -> [b] -> [b]
mergeBy by upd lhs rhs =
  elems
    $ mergeMap
      upd
      (fromList $ (\x -> (by x, x)) <$> lhs)
      (fromList $ (\x -> (by x, x)) <$> rhs)

asumMap ::
  forall a b f m.
  ( Element (f a) ~ a,
    Container (f a),
    Monoid (m b)
  ) =>
  (a -> m b) ->
  f a ->
  m b
asumMap f =
  foldr (mappend . f) mempty

altM :: (Monad m) => (a -> m (Either e b)) -> NonEmpty a -> m (Either e b)
altM f (x :| xs) = either (altM' f xs) (pure . Right) =<< f x

altM' :: (Monad m) => (a -> m (Either e b)) -> [a] -> e -> m (Either e b)
altM' _ [] e = pure $ Left e
altM' f [x] _ = f x
altM' f (x : xs) _ = either (altM' f xs) (pure . Right) =<< f x

takeWhileAcc :: (a -> b -> Maybe b) -> b -> [a] -> [a]
takeWhileAcc _ _ [] = []
takeWhileAcc f prev (x : xs) =
  case f x prev of
    Nothing -> []
    Just next -> x : takeWhileAcc f next xs

--
-- NOTE : compat with older random
--

randomList :: forall a m. (Random.Random a, MonadIO m) => Natural -> m [a]
randomList = liftIO . Random.getStdRandom . randomListPure

randomListPure ::
  (Random.RandomGen g, Random.Random a) => Natural -> g -> ([a], g)
randomListPure qty =
  go mempty qty
  where
    go acc 0 g = (acc, g)
    go acc n prev =
      let (x, next) = Random.random prev
       in go (x : acc) (n - 1) next

randomByteString :: (MonadIO m) => Natural -> m ByteString
randomByteString = liftIO . Random.getStdRandom . randomByteStringPure

randomByteStringPure :: (Random.RandomGen g) => Natural -> g -> (ByteString, g)
randomByteStringPure qty =
  first BS.pack
    . randomListPure qty

expBackOff :: forall a. (From a Natural) => a -> Natural
expBackOff = (2 ^) . from @a @Natural

expBackOffSecondsAfter :: (From a Natural) => a -> UTCTime -> UTCTime
expBackOffSecondsAfter attempt =
  addUTCTime
    ( secondsToNominalDiffTime
        $ expBackOff attempt
    )

secondsToNominalDiffTime :: forall a. (From a Natural) => a -> NominalDiffTime
secondsToNominalDiffTime =
  Universum.fromIntegral
    . from @a @Natural

nubOrdNE :: (Ord a) => NonEmpty a -> NonEmpty a
nubOrdNE = nubOrdByNE compare

nubOrdByNE :: (a -> a -> Ordering) -> NonEmpty a -> NonEmpty a
nubOrdByNE cmp = fromList . nubOrdBy cmp . toList

nubOrdOnNE :: (Ord b) => (a -> b) -> NonEmpty a -> NonEmpty a
nubOrdOnNE f = fromList . nubOrdOn f . toList

enumerateNE :: forall a. (Ord a, Enum a, Bounded a) => NonEmpty a
enumerateNE = nubOrdNE $ minBound :| enumerate @a

nextEnum :: (Eq a, Enum a, Bounded a) => a -> a
nextEnum x
  | x == maxBound = minBound
  | otherwise = succ x

prevEnum :: (Eq a, Enum a, Bounded a) => a -> a
prevEnum x
  | x == minBound = maxBound
  | otherwise = pred x

type Textual mono =
  ( Typeable mono,
    Container mono,
    Seq.Textual mono,
    From String mono,
    From Text mono,
    From TL.Text mono,
    From Unicode mono,
    ConvertUtf8 mono ByteString,
    ConvertUtf8 mono BL.ByteString
  )

strip :: (Textual mono) => mono -> mono
strip =
  dropAround Char.isSpace

type Mono a mono =
  ( a ~ Mono.Element mono,
    Seq.IsSequence mono,
    Container mono
  )

dropAround :: (Mono a mono) => (a -> Bool) -> mono -> mono
dropAround f =
  Seq.dropWhile f . dropWhileEnd f

dropWhileEnd :: (Mono a mono) => (a -> Bool) -> mono -> mono
dropWhileEnd f =
  Mono.ofoldr
    ( \x xs ->
        if f x && null xs
          then mempty
          else Seq.cons x xs
    )
    mempty

-- $derivingVia
-- Newtypes to simplify deriving via.
-- We have to expose default constructors/accessors
-- to help GHC with figuring out that runtime representation does match.

newtype GenericType a = GenericType
  { unGenericType :: a
  }
  deriving stock (Generic)

newtype GenericEnum a = GenericEnum
  { unGenericEnum :: a
  }
  deriving newtype (Show, Enum, Bounded)
