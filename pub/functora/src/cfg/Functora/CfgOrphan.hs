{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Functora.CfgOrphan (genericTomlCodec) where

import Data.Aeson
  ( FromJSON (..),
    FromJSONKey (..),
    ToJSON (..),
    ToJSONKey (..),
  )
import qualified Data.Aeson as A
import qualified Data.Binary as Binary
import Data.Binary.Instances ()
import Functora.Prelude
import qualified GHC.Generics as Generics
import qualified Text.URI as URI
import Toml (HasCodec, HasItemCodec)
import qualified Toml
#if defined(__GHCJS__) || defined(ghcjs_HOST_OS) || defined(wasi_HOST_OS)
import Data.JSString (JSString)
#endif

genericTomlCodec ::
  ( Generic a,
    Typeable a,
    Toml.GenericCodec (Rep a)
  ) =>
  Toml.TomlCodec a
genericTomlCodec =
  Toml.genericCodecWithOptions
    Toml.TomlOptions
      { Toml.tomlOptionsFieldModifier = \proxy ->
          Toml.stripTypeNamePrefix proxy . \case
            ('_' : xs) -> xs
            xs -> xs
      }

instance A.ToJSON URI where
  toJSON =
    A.toJSON . URI.render

instance A.FromJSON URI where
  parseJSON =
    A.withText "URI"
      $ either (fail . displayException) pure
      . URI.mkURI

instance Toml.HasCodec URI where
  hasCodec =
    Toml.textBy URI.render
      $ first (from @String @Text . displayException)
      . URI.mkURI

instance Toml.HasItemCodec URI where
  hasItemCodec =
    Left
      . Toml.mkAnyValueBiMap
        ( \src -> do
            txt <- Toml.matchText src
            first
              (const . Toml.MatchError Toml.TText $ Toml.AnyValue src)
              $ URI.mkURI txt
        )
      $ Toml.Text
      . URI.render

instance Toml.HasCodec UTCTime where
  hasCodec =
    Toml.textBy (from @String @Text . iso8601Show)
      $ maybe (Left "Not a UTCTime") pure
      . iso8601ParseM
      . from @Text @String

instance Toml.HasItemCodec UTCTime where
  hasItemCodec =
    Left
      . Toml.mkAnyValueBiMap
        ( \src -> do
            txt <- Toml.matchText src
            maybe
              (Left . Toml.MatchError Toml.TText $ Toml.AnyValue src)
              pure
              . iso8601ParseM
              $ from @Text @String txt
        )
      $ Toml.Text
      . from @String @Text
      . iso8601Show

instance (Show a, Enum a, Bounded a) => Toml.HasCodec (GenericEnum a) where
  hasCodec = Toml.enumBounded

instance (Show a, Enum a, Bounded a) => Toml.HasItemCodec (GenericEnum a) where
  hasItemCodec = Left Toml._EnumBounded

instance
  ( Generic a,
    Typeable a,
    Toml.GenericCodec (Rep a)
  ) =>
  Toml.HasCodec (GenericType a)
  where
  hasCodec = Toml.diwrap . Toml.table (genericTomlCodec @a)

instance
  ( Generic a,
    Typeable a,
    Toml.GenericCodec (Rep a)
  ) =>
  Toml.HasItemCodec (GenericType a)
  where
  hasItemCodec = Right . Toml.diwrap $ genericTomlCodec @a

instance
  ( Generic a,
    Typeable a,
    A.GFromJSON A.Zero (Rep a)
  ) =>
  FromJSON (GenericType a)
  where
  parseJSON = fmap GenericType . A.genericParseJSON (optsAeson @a)

instance
  ( Generic a,
    Typeable a,
    A.GFromJSON A.Zero (Rep a),
    A.GFromJSONKey (Rep a)
  ) =>
  FromJSONKey (GenericType a)
  where
  fromJSONKey = GenericType <$> A.genericFromJSONKey A.defaultJSONKeyOptions

instance
  ( Generic a,
    Typeable a,
    A.GToJSON A.Zero (Rep a),
    A.GToEncoding A.Zero (Rep a)
  ) =>
  ToJSON (GenericType a)
  where
  toJSON = A.genericToJSON (optsAeson @a) . unGenericType
  toEncoding = A.genericToEncoding (optsAeson @a) . unGenericType

instance
  ( Generic a,
    Typeable a,
    A.GToJSON A.Zero (Rep a),
    A.GToEncoding A.Zero (Rep a),
    A.GToJSONKey (Rep a)
  ) =>
  ToJSONKey (GenericType a)
  where
  toJSONKey = contramap unGenericType $ A.genericToJSONKey A.defaultJSONKeyOptions

instance
  ( Generic a,
    Typeable a,
    Binary.GBinaryPut (Rep a),
    Binary.GBinaryGet (Rep a)
  ) =>
  Binary (GenericType a)
  where
  putList = defaultPutList
  put = Binary.gput . Generics.from . unGenericType
  get = GenericType . Generics.to <$> Binary.gget

instance (Toml.HasCodec a) => Toml.HasCodec (Tagged t a) where
  hasCodec =
    Toml.diwrap . Toml.hasCodec @a

instance (Toml.HasItemCodec a) => Toml.HasItemCodec (Tagged t a) where
  hasItemCodec =
    bimap Toml._Coerce Toml.diwrap $ Toml.hasItemCodec @a

_Ratio ::
  forall a.
  ( Integral a,
    From a Integer,
    TryFrom Integer a
  ) =>
  Toml.TomlBiMap (Ratio a) Toml.AnyValue
_Ratio =
  Toml.mkAnyValueBiMap
    ( \src -> do
        let failure =
              Toml.MatchError Toml.TDouble
                $ Toml.AnyValue src
        dbl <-
          Toml.matchDouble src
        rat <-
          first (const failure)
            $ tryFrom @Double @Rational dbl
        first (const failure)
          $ tryFrom @Rational @(Ratio a) rat
    )
    ( Toml.Double
        . via @Rational @(Ratio a) @Double
    )

--
-- TODO : how to make an instance for a Rational nicely?
--
instance
  ( Integral a,
    From a Integer,
    TryFrom Integer a
  ) =>
  Toml.HasCodec (Ratio a)
  where
  hasCodec = Toml.match $ _Ratio @a

instance
  ( Integral a,
    From a Integer,
    TryFrom Integer a
  ) =>
  Toml.HasItemCodec (Ratio a)
  where
  hasItemCodec = Left $ _Ratio @a

{-# INLINE defaultPutList #-}
defaultPutList :: (Binary a) => [a] -> Binary.Put
defaultPutList xs = Binary.put (length xs) <> mapM_ Binary.put xs

optsAeson :: forall a. (Typeable a) => A.Options
optsAeson =
  A.defaultOptions
    { A.fieldLabelModifier = \case
        raw@('_' : inp) ->
          case fmt inp of
            out | out == inp -> fmt raw
            out -> out
        raw ->
          fmt raw,
      A.constructorTagModifier = id,
      A.allNullaryToStringTag = True,
      A.omitNothingFields = True,
      A.sumEncoding = A.defaultTaggedObject,
      A.unwrapUnaryRecords = False,
      A.tagSingleConstructors = False
    }
  where
    fmt = Toml.stripTypeNamePrefix $ Proxy @a

#if defined(__GHCJS__) || defined(ghcjs_HOST_OS) || defined(wasi_HOST_OS)
instance Toml.HasCodec JSString where
  hasCodec =
    Toml.textBy (from @JSString @Text)
      $ pure . from @Text @JSString

instance Toml.HasItemCodec JSString where
  hasItemCodec =
    Left
      . Toml.mkAnyValueBiMap
        ( \src -> do
            txt <- Toml.matchText src
            pure $ from @Text @JSString txt
        )
      $ Toml.Text
      . from @JSString @Text
#endif

deriving via GenericEnum AscOrDesc instance HasCodec AscOrDesc

deriving via GenericEnum AscOrDesc instance HasItemCodec AscOrDesc
