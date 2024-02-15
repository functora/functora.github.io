module Functora.Prelude
  ( -- * Reexport
    -- $reexport
    module X,
    LiftTH,
    getCurrentTime,

    -- * Show
    -- $show
    inspect,
    inspectType,
    inspectSymbol,
    inspectRatio,

    -- * Integral
    -- $integral
    safeFromIntegral,

    -- * Lens
    -- $lens
    view,
    (^.),
    mkGetters,

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

    -- * Exceptions
    -- $exceptions
    ParseException,
    throwParseException,
    throwString,

    -- * Parsers
    -- $parsers
    parseWords,
    parseRatio,

    -- * QQ
    -- $qq
    qq,
    qqUri,

    -- * Merge
    -- $merge
    mergeMap,
    mergeAlt,
    mergeBy,
    asumMap,
    altM,
    altM',
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
import Control.Exception.Safe as X (throw)
import qualified Control.Exception.Safe as Safe
import Control.Lens.Combinators as X (first1Of, makePrisms, review)
import Control.Monad.Extra as X
  ( eitherM,
    fromMaybeM,
    maybeM,
  )
import Control.Monad.Trans.Chronicle as X (ChronicleT (..), chronicle)
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Base16.Lazy as BL16
import qualified Data.ByteString.Lazy as BL
import Data.Either.Extra as X (fromEither)
import Data.Fixed as X (Pico)
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
import Data.List.Extra as X (enumerate, notNull, nubOrd, nubOrdOn)
import qualified Data.Map.Merge.Strict as Map
import Data.Ratio as X ((%))
import Data.Scientific as X (Scientific)
import qualified Data.Semigroup as Semi
import Data.Tagged as X (Tagged (..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text.Read as T
import Data.These as X (These (..), these)
import Data.These.Combinators as X (hasThere)
import Data.These.Lens as X
import Data.Time.Clock as X (UTCTime (..), diffUTCTime)
import qualified Data.Time.Clock as Clock
import Data.Tuple.Extra as X (uncurry3)
import qualified Data.Typeable as Typeable
import Functora.PreludeOrphan as X ()
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
    catch,
    catchAny,
    finally,
    fromInteger,
    fromIntegral,
    handleAny,
    on,
    over,
    preview,
    print,
    set,
    show,
    state,
    swap,
    throwM,
    try,
    tryAny,
    view,
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

-- $reexport
-- Reexport

type LiftTH = TH.Lift

getCurrentTime :: (MonadIO m) => m UTCTime
getCurrentTime = liftIO Clock.getCurrentTime

-- $show
-- Show

inspect :: forall dst src. (Show src, Data src, IsString dst) => src -> dst
inspect =
  display @dst @src
    . Syb.everywhere (Syb.mkT prettyByteString)
    . Syb.everywhere (Syb.mkT prettyLazyByteString)

inspectType :: forall a b. (Typeable a, IsString b) => b
inspectType =
  Universum.show
    . Typeable.typeRep
    $ Proxy @a

inspectSymbol :: forall a b. (KnownSymbol a, IsString b) => b
inspectSymbol =
  fromString
    . TypeLits.symbolVal
    $ Proxy @a

inspectRatio ::
  forall a b.
  ( From String a,
    Integral b,
    Show b
  ) =>
  Int ->
  Ratio b ->
  a
inspectRatio decimalPlaces signedRational =
  let (quotient, remainder) = unsignedNumerator `quotRem` unsignedDenominator
      fractionalPart = take decimalPlaces (go remainder)
   in from @String @a
        $ (if signedRational < 0 then "-" else mempty)
        <> Prelude.shows
          quotient
          (if null fractionalPart then mempty else "." <> fractionalPart)
  where
    unsignedRational = abs signedRational
    unsignedNumerator = numerator unsignedRational
    unsignedDenominator = denominator unsignedRational
    go 0 = mempty
    go previous =
      let (current, next) = (10 * previous) `quotRem` unsignedDenominator
       in Prelude.shows current (go next)

display :: forall dst src. (Show src, Typeable src, IsString dst) => src -> dst
display x
  | Just HRefl <- typeOf x `eqTypeRep` typeRep @String =
      fromString x
  | Just HRefl <- typeOf x `eqTypeRep` typeRep @Text =
      fromString $ from @Text @String x
  | Just HRefl <- typeOf x `eqTypeRep` typeRep @TL.Text =
      fromString $ from @TL.Text @String x
  | Just HRefl <- typeOf x `eqTypeRep` typeRep @ByteString =
      either
        (const $ Universum.show x)
        (fromString . from @Text @String)
        (TE.decodeUtf8' x)
  | Just HRefl <- typeOf x `eqTypeRep` typeRep @BL.ByteString =
      either
        (const $ Universum.show x)
        (fromString . from @TL.Text @String)
        (TLE.decodeUtf8' x)
  | otherwise =
      Universum.show x

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

view :: Getting (Semi.First a) s a -> s -> a
view = first1Of
{-# INLINE view #-}

infixl 8 ^.

(^.) :: s -> Getting (Semi.First a) s a -> a
(^.) = flip first1Of
{-# INLINE (^.) #-}

mkGetters :: TH.Name -> TH.DecsQ
mkGetters =
  TH.makeLensesWith $ TH.lensRules & TH.generateUpdateableOptics .~ False

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

-- $exceptions
-- Exceptions

data ParseException = ParseException
  { parseExceptionSource :: Text,
    parseExceptionSourceType :: SomeTypeRep,
    parseExceptionTargetType :: SomeTypeRep,
    parseExceptionFailure :: Text
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

parseWords :: forall a. (From a Text) => a -> [Text]
parseWords = filter (not . null) . T.splitOn " " . from @a @Text

parseRatio ::
  forall str int m.
  ( From str Text,
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
    Right (rat, "") -> pure rat
    Right e -> throwParseException str e
    Left e -> throwParseException str e

-- $qq
-- QQ

qq ::
  forall inp out e.
  ( From String inp,
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
    failure :: Text -> any
    failure msg =
      error
        $ inspectType @out
        <> " "
        <> msg
        <> " is not implemented"

qqUri :: QuasiQuoter
qqUri = URI.uri

-- $merge
-- Merge

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
