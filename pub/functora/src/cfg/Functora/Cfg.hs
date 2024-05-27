{-# LANGUAGE UndecidableInstances #-}

module Functora.Cfg
  ( module X,

    -- * Cli
    -- $cli
    Cli (..),
    newCli,

    -- * JSON
    -- $json
    decodeJson,
    encodeJson,

    -- * TOML
    -- $toml
    decodeToml,
    encodeToml,

    -- * CBOR
    -- $cbor
    decodeCbor,
    encodeCbor,

    -- * Binary
    -- $binary
    decodeBinary,
    encodeBinary,

    -- * DerivingVia
    -- $derivingVia
    GenericEnum (..),
    GenericType (..),
  )
where

import Codec.Serialise as X (Serialise)
import qualified Codec.Serialise as Cbor
import qualified Codec.Serialise.Class as Cbor
import Data.Aeson as X
  ( FromJSON (..),
    FromJSONKey (..),
    ToJSON (..),
    ToJSONKey (..),
  )
import qualified Data.Aeson as A
import Data.Binary as X (Binary)
import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import Data.Binary.Instances as X ()
import qualified Data.ByteString.Lazy as BL
import qualified Data.List.NonEmpty as NE
import Functora.CfgOrphan as X ()
import Functora.Prelude
import qualified GHC.Generics as Generics
import qualified Options.Applicative as Cli
import Toml as X
  ( HasCodec,
    HasItemCodec,
    TomlCodec,
    TomlDecodeError (..),
    prettyTomlDecodeErrors,
  )
import qualified Toml

-- $cli
-- Cli

data Cli
  = CliTextConf (NonEmpty Text) Bool
  | CliFileConf (NonEmpty FilePath) Bool

newCli :: (MonadIO m) => m Cli
newCli =
  liftIO $ Cli.execParser cliParser

cliParser :: Cli.ParserInfo Cli
cliParser =
  Cli.info ((textConf <|> fileConf) <**> Cli.helper)
    $ Cli.fullDesc
    <> Cli.header mempty
    <> Cli.progDesc "Cli parser"

textConf :: Cli.Parser Cli
textConf =
  CliTextConf
    <$> fmap
      NE.fromList
      ( some
          . Cli.strOption
          $ Cli.long "text-conf"
          <> Cli.short 't'
          <> Cli.metavar "TEXTCONF"
          <> Cli.help "Config as plain text"
      )
    <*> showConf

fileConf :: Cli.Parser Cli
fileConf =
  CliFileConf
    <$> fmap
      NE.fromList
      ( some
          . Cli.strOption
          $ Cli.long "file-conf"
          <> Cli.short 'f'
          <> Cli.metavar "FILENAME"
          <> Cli.help "Config file location"
      )
    <*> showConf

showConf :: Cli.Parser Bool
showConf =
  Cli.switch
    ( Cli.long "show-conf"
        <> Cli.short 's'
        <> Cli.help "UNSAFE SHOW CONFIG INCLUDING SECRETS"
    )

-- $json
-- JSON

decodeJson ::
  forall inp out.
  ( FromJSON out,
    From inp (UTF_8 BL.ByteString)
  ) =>
  inp ->
  Either String out
decodeJson raw =
  A.eitherDecode
    $ via @(UTF_8 BL.ByteString) @inp @BL.ByteString raw

encodeJson :: (ToJSON a) => a -> UTF_8 BL.ByteString
encodeJson =
  Tagged @"UTF-8"
    . A.encode

-- $toml
-- TOML

decodeToml ::
  ( Generic a,
    Typeable a,
    Toml.GenericCodec (Rep a)
  ) =>
  Text ->
  Either [Toml.TomlDecodeError] a
decodeToml =
  Toml.decodeExact genericTomlCodec

encodeToml ::
  ( Generic a,
    Typeable a,
    Toml.GenericCodec (Rep a)
  ) =>
  a ->
  Text
encodeToml =
  Toml.encode genericTomlCodec

genericTomlCodec ::
  ( Generic a,
    Typeable a,
    Toml.GenericCodec (Rep a)
  ) =>
  TomlCodec a
genericTomlCodec =
  Toml.genericCodecWithOptions
    Toml.TomlOptions
      { Toml.tomlOptionsFieldModifier = \proxy ->
          Toml.stripTypeNamePrefix proxy . \case
            ('_' : xs) -> xs
            xs -> xs
      }

-- $cbor
-- CBOR

decodeCbor ::
  forall inp out m.
  ( From inp BL.ByteString,
    Serialise out,
    MonadThrow m
  ) =>
  inp ->
  m out
decodeCbor =
  either throw pure
    . Cbor.deserialiseOrFail
    . from @inp @BL.ByteString

encodeCbor :: (Serialise a) => a -> BL.ByteString
encodeCbor = Cbor.serialise

instance
  ( Generic a,
    Typeable a,
    Cbor.GSerialiseEncode (Rep a),
    Cbor.GSerialiseDecode (Rep a)
  ) =>
  Serialise (GenericType a)
  where
  encode = Cbor.gencode . Generics.from . unGenericType
  decode = GenericType . Generics.to <$> Cbor.gdecode

-- $binary
-- Binary

decodeBinary ::
  forall inp out.
  ( From inp BL.ByteString,
    Show out,
    Data out,
    Binary out
  ) =>
  inp ->
  Either (BL.ByteString, Binary.ByteOffset, String) out
decodeBinary raw = do
  (extra, qty, res) <- Binary.decodeOrFail $ from @inp @BL.ByteString raw
  if null extra
    then pure res
    else Left (extra, qty, inspect res)

encodeBinary :: (Binary a) => a -> BL.ByteString
encodeBinary = Binary.encode

-- $derivingVia
-- Newtypes to simplify deriving via.
-- We have to expose default constructors/accessors
-- to help GHC with figuring out that runtime representation does match.

newtype GenericEnum a = GenericEnum
  { unGenericEnum :: a
  }
  deriving newtype (Show, Enum, Bounded)

instance (Show a, Enum a, Bounded a) => HasCodec (GenericEnum a) where
  hasCodec = Toml.enumBounded

instance (Show a, Enum a, Bounded a) => HasItemCodec (GenericEnum a) where
  hasItemCodec = Left Toml._EnumBounded

newtype GenericType a = GenericType
  { unGenericType :: a
  }
  deriving stock (Generic)

instance
  ( Generic a,
    Typeable a,
    Toml.GenericCodec (Rep a)
  ) =>
  HasCodec (GenericType a)
  where
  hasCodec = Toml.diwrap . Toml.table (genericTomlCodec @a)

instance
  ( Generic a,
    Typeable a,
    Toml.GenericCodec (Rep a)
  ) =>
  HasItemCodec (GenericType a)
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
