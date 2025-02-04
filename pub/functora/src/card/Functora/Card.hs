{-# LANGUAGE OverloadedLabels #-}

module Functora.Card (main) where

import qualified Codec.Picture as CP
import qualified Codec.Picture.Types as CPT
import qualified Codec.QRCode as QR
import qualified Codec.QRCode.JuicyPixels as QRJP
import qualified Data.ByteString.Lazy as BL
import Functora.Cfg
import Functora.Prelude
import qualified Graphics.Rasterific as R
import qualified Graphics.Rasterific.Texture as RT
import qualified Graphics.Text.TrueType as TT
import qualified Numeric as N
import qualified Prelude

main :: IO ()
main = withUtf8 $ do
  cli <- newCli
  cfg <-
    --
    -- TODO : generalize this and move to Functora.Cfg
    --
    fmap (foldl1 const) $ case cli of
      CliTextConf txt _ -> forM txt unToml
      CliFileConf loc _ -> forM loc $ readFile >=> unToml
  putStrLn $ inspect @Text @Cfg cfg
  headFont <- mkFont $ cfgHeadFont cfg
  iconFont <- mkFont $ cfgIconFont cfg
  noteFont <- mkFont $ cfgNoteFont cfg
  CP.writePng "./img/functora.png"
    $ mkCard
      Env
        { envDpi = cfgDpi cfg,
          envWidth = cfgWidth cfg,
          envHeight = cfgHeight cfg,
          envPadding = cfgPadding cfg,
          envHeadFont = headFont,
          envIconFont = iconFont,
          envNoteFont = noteFont,
          envGroup = cfgGroup cfg
        }

mkFont :: Text -> IO TT.Font
mkFont =
  BL.readFile
    . from @Text @String
    >=> either throwString pure
    . TT.decodeFont

mkCard :: Env -> CP.Image CP.PixelRGBA8
mkCard env =
  R.renderDrawingAtDpi
    (round $ envWidth env)
    (round $ envHeight env)
    (envDpi env)
    white
    . forM_ (zip [0 ..] $ envGroup env)
    . uncurry
    $ mkGroup env (length $ envGroup env)

mkGroup :: Env -> Int -> Int -> Group -> R.Drawing CPT.PixelRGBA8 ()
mkGroup env amt idx (Group items) =
  foldM_ (mkItem env offX) 0 items
  where
    pad = envPadding env
    offX =
      pad
        + unsafeFrom @Int @Float idx
        * (envWidth env / unsafeFrom @Int @Float amt)

mkItem ::
  Env ->
  Float ->
  Float ->
  Item ->
  R.Drawing CPT.PixelRGBA8 Float
mkItem env offX offY item =
  case itemKind item of
    Head -> mkText env offX offY item $ envHeadFont env
    Icon -> mkText env offX offY next $ envIconFont env
    Note -> mkText env offX offY item $ envNoteFont env
    Qr -> mkQr env offX offY item
  where
    next = item & #itemData %~ mkHex . from @Text @String

mkText ::
  Env ->
  Float ->
  Float ->
  Item ->
  TT.Font ->
  R.Drawing CPT.PixelRGBA8 Float
mkText env offX offY item font = do
  R.withTexture black
    . R.printTextAt
      font
      (TT.pixelSizeInPointAtDpi size $ envDpi env)
      (R.V2 offX offN)
    $ from @Text @String text
  pure offN
  where
    text = itemData item
    size = itemSize item
    offN =
      offY
        + envPadding env
        + size

mkQr ::
  Env ->
  Float ->
  Float ->
  Item ->
  R.Drawing CPT.PixelRGBA8 Float
mkQr env offX offY item = do
  R.drawImage img 0 $ R.V2 offX offY
  pure $ offY + envPadding env + px
  where
    px = itemSize item
    qr =
      fromMaybe (error "Can not generate qr!")
        . QR.encodeAutomatic
          (QR.defaultQRCodeOptions QR.L)
          QR.Iso8859_1OrUtf8WithoutECI
        $ itemData item
    img =
      CPT.promoteImage
        $ QRJP.toImage 0 (round px `div` QR.qrImageSize qr) qr

data Cfg = Cfg
  { cfgDpi :: Int,
    cfgWidth :: Float,
    cfgHeight :: Float,
    cfgPadding :: Float,
    cfgHeadFont :: Text,
    cfgIconFont :: Text,
    cfgNoteFont :: Text,
    cfgGroup :: [Group]
  }
  deriving stock
    ( Eq,
      Ord,
      Show,
      Data,
      Generic
    )
  deriving
    ( HasCodec,
      HasItemCodec
    )
    via GenericType Cfg

data RowOrCol
  = Row
  | Col
  deriving stock
    ( Eq,
      Ord,
      Show,
      Data,
      Generic,
      Enum,
      Bounded
    )
  deriving
    ( HasCodec,
      HasItemCodec
    )
    via GenericEnum RowOrCol

newtype Group = Group
  { groupItem :: [Item]
  }
  deriving stock
    ( Eq,
      Ord,
      Show,
      Data,
      Generic
    )
  deriving
    ( HasCodec,
      HasItemCodec
    )
    via GenericType Group

data Item = Item
  { itemKind :: ItemKind,
    itemData :: Text,
    itemSize :: Float
  }
  deriving stock
    ( Eq,
      Ord,
      Show,
      Data,
      Generic
    )
  deriving
    ( HasCodec,
      HasItemCodec
    )
    via GenericType Item

data ItemKind
  = Head
  | Icon
  | Note
  | Qr
  deriving stock
    ( Eq,
      Ord,
      Show,
      Data,
      Generic,
      Bounded,
      Enum
    )
  deriving
    ( HasCodec,
      HasItemCodec
    )
    via GenericEnum ItemKind

data Env = Env
  { envDpi :: Int,
    envWidth :: Float,
    envHeight :: Float,
    envPadding :: Float,
    envHeadFont :: TT.Font,
    envIconFont :: TT.Font,
    envNoteFont :: TT.Font,
    envGroup :: [Group]
  }
  deriving stock (Show, Generic)

white :: CPT.PixelRGBA8
white = CP.PixelRGBA8 255 255 255 255

black :: RT.Texture CPT.PixelRGBA8
black = RT.uniformTexture $ CP.PixelRGBA8 0 0 0 255

mkHex :: String -> Text
mkHex ('0' : 'x' : hexStr) =
  pack [chr (fst . Prelude.head $ N.readHex hexStr)]
mkHex hexStr =
  pack [chr (fst . Prelude.head $ N.readHex hexStr)]
