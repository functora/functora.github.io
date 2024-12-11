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
    unToml,
    decodeToml,
    encodeToml,

    -- * Binary
    -- $binary
    decodeBinary,
    encodeBinary,
  )
where

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
import Functora.CfgOrphan (genericTomlCodec)
import Functora.Prelude
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

encodeJson ::
  forall inp out.
  ( ToJSON inp,
    From (UTF_8 BL.ByteString) out
  ) =>
  inp ->
  out
encodeJson =
  from @(UTF_8 BL.ByteString) @out
    . Tagged @"UTF-8"
    . A.encode

-- $toml
-- TOML

unToml ::
  ( Generic a,
    Typeable a,
    Toml.GenericCodec (Rep a),
    MonadThrow m
  ) =>
  Text ->
  m a
unToml =
  either
    ( throwString
        . prettyTomlDecodeErrors
    )
    pure
    . decodeToml

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
