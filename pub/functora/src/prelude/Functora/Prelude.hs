module Functora.Prelude
  ( module X,

    -- * Show
    -- $show
    inspect,
    inspectType,
    inspectSymbol,

    -- * Integral
    -- $integral
    safeFromIntegral,

    -- * Lens
    -- $lens
    view,
    (^.),
    makeGetters,

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
import Control.Lens.Combinators as X (first1Of, makePrisms)
import Control.Monad.Except as X (MonadError (..))
import Control.Monad.Extra as X
  ( eitherM,
    fromMaybeM,
    maybeM,
  )
import Control.Monad.Trans.Chronicle as X (ChronicleT (..), chronicle)
import Control.Monad.Trans.Except as X
  ( catchE,
    except,
    mapExceptT,
    throwE,
    withExceptT,
  )
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Base16.Lazy as BL16
import qualified Data.ByteString.Lazy as BL
import Data.Either.Extra as X (fromEither)
import Data.Functor.Contravariant as X (contramap)
import Data.Generics as X (Data)
import qualified Data.Generics as Syb
import Data.List.Extra as X (enumerate, notNull)
import Data.Scientific as X (Scientific)
import qualified Data.Semigroup as Semi
import Data.Tagged as X (Tagged (..))
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Data.These as X (These (..), these)
import Data.These.Combinators as X (hasThere)
import Data.These.Lens as X
import Data.Time.Clock as X (UTCTime, getCurrentTime)
import Data.Time.Format.ISO8601 as X (ISO8601, iso8601Show)
import qualified Data.Typeable as Typeable
import Functora.PreludeOrphan as X ()
import GHC.Generics as X (Rep)
import qualified GHC.TypeLits as TypeLits
import qualified Language.Haskell.TH.Lib as TH
import qualified Language.Haskell.TH.Syntax as TH
import Lens.Micro as X hiding ((^.))
import Lens.Micro.Contra as X (Fold, Getter)
import Lens.Micro.GHC as X ()
import Lens.Micro.TH as X (makeLenses)
import qualified Lens.Micro.TH as TH
  ( generateUpdateableOptics,
    lensRules,
    makeLensesWith,
  )
import Main.Utf8 as X (withUtf8)
import Text.URI as X (URI)
import Text.URI.QQ as X (uri)
import Type.Reflection
import Universum as X hiding
  ( Lens,
    Lens',
    Traversal,
    Traversal',
    atomically,
    bracket,
    finally,
    fromInteger,
    fromIntegral,
    on,
    over,
    print,
    set,
    show,
    state,
    swap,
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
    finally,
    race,
    toIO,
    withRunInIO,
    withUnliftIO,
  )
import Witch.Mini as X
import qualified Prelude

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

inspectSymbol :: forall a b. (TypeLits.KnownSymbol a, IsString b) => b
inspectSymbol =
  fromString
    . TypeLits.symbolVal
    $ Proxy @a

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

makeGetters :: TH.Name -> TH.DecsQ
makeGetters =
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
