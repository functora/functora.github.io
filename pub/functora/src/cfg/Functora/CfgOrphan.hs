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
import qualified Toml

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
