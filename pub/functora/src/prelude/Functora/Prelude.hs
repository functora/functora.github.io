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

    -- * DerivingVia
    -- $derivingVia
    Redacted (..),
  )
where

import Control.Lens.Combinators as X (Getting, first1Of, makePrisms)
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Base16.Lazy as BL16
import qualified Data.ByteString.Lazy as BL
import Data.Functor.Contravariant as X (contramap)
import Data.Generics as X (Data)
import qualified Data.Generics as Syb
import qualified Data.Semigroup as Semi
import Data.Tagged as X (Tagged (..))
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Typeable as Typeable
import GHC.Generics as X (Rep)
import qualified GHC.TypeLits as TypeLits
import Type.Reflection
import Universum as X hiding
  ( atomically,
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
    (^.),
  )
import qualified Universum
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

-- $derivingVia
-- Newtypes to simplify deriving via.
-- We have to expose default constructors/accessors
-- to help GHC with figuring out that runtime representation does match.

newtype Redacted a = Redacted
  { unRedacted :: a
  }

instance (Typeable a) => Show (Redacted a) where
  show = const $ "<REDACTED> :: " <> inspectType @a
