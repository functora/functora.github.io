{-# LANGUAGE OverloadedLabels #-}

module Functora.Card (main) where

import qualified Codec.Picture as CP
import qualified Codec.Picture.Extra as JP
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
  headFont <- mkFont $ cfg ^. #cfgFont . #cfgFontHead
  iconFont <- mkFont $ cfg ^. #cfgFont . #cfgFontIcon
  noteFont <- mkFont $ cfg ^. #cfgFont . #cfgFontNote
  let img =
        mkCard
          Env
            { envImg = cfgImg cfg,
              envFont =
                Font
                  { fontHead = headFont,
                    fontIcon = iconFont,
                    fontNote = noteFont
                  },
              envGroup = cfgGroup cfg
            }
  let width = cfg ^. #cfgImg . #imgWidth . #unPx
  let height = cfg ^. #cfgImg . #imgHeight . #unPx
  forM_ (Extra (Px width) (Px height) False : cfgExtra cfg) $ \ext -> do
    let extWidth = ext ^. #extraWidth . #unPx
    let extHeight = ext ^. #extraHeight . #unPx
    let fin =
          R.renderDrawingAtDpi
            extWidth
            extHeight
            (cfg ^. #cfgImg . #imgDpi . #unPx)
            white
            . R.drawImage img 0
            $ R.V2
              (unsafeFrom @Int @Float (extWidth - width) / 2)
              (unsafeFrom @Int @Float (extHeight - height) / 2)
    if extraVertical ext
      then
        CP.writePng
          ( "./img/card-"
              <> inspect extHeight
              <> "x"
              <> inspect extWidth
              <> ".png"
          )
          $ JP.rotateRight90 fin
      else
        CP.writePng
          ( "./img/card-"
              <> inspect extWidth
              <> "x"
              <> inspect extHeight
              <> ".png"
          )
          fin

mkFont :: Text -> IO TT.Font
mkFont =
  BL.readFile
    . from @Text @String
    >=> either throwString pure
    . TT.decodeFont

mkCard :: Env -> CP.Image CP.PixelRGBA8
mkCard env =
  R.renderDrawingAtDpi
    (env ^. #envImg . #imgWidth . #unPx)
    (env ^. #envImg . #imgHeight . #unPx)
    (env ^. #envImg . #imgDpi . #unPx)
    white
    . forM_ (zip [0 ..] $ envGroup env)
    . uncurry
    $ mkGroup env (length $ envGroup env)

mkGroup :: Env -> Int -> Int -> Group -> R.Drawing CPT.PixelRGBA8 ()
mkGroup env amt idx (Group items) =
  foldM_ (mkItem env offX) (Px 0) items
  where
    offX =
      Px
        . round
        $ via @Integer @Int @Rational (env ^. #envImg . #imgPadX . #unPx)
        + via @Integer @Int @Rational idx
        * ( via @Integer @Int @Rational
              ( (env ^. #envImg . #imgWidth . #unPx)
                  - (env ^. #envImg . #imgPadX . #unPx)
              )
              / via @Integer @Int @Rational amt
          )

mkItem ::
  Env ->
  Px ->
  Px ->
  Item ->
  R.Drawing CPT.PixelRGBA8 Px
mkItem env offX offY item =
  case itemKind item of
    Head -> mkText env offX offY item $ env ^. #envFont . #fontHead
    Icon -> mkText env offX offY next $ env ^. #envFont . #fontIcon
    Note -> mkText env offX offY item $ env ^. #envFont . #fontNote
    Qr -> mkQr env offX offY item
  where
    next = item & #itemData %~ mkHex . from @Text @String

mkText ::
  Env ->
  Px ->
  Px ->
  Item ->
  TT.Font ->
  R.Drawing CPT.PixelRGBA8 Px
mkText env offX offY item font = do
  R.withTexture black
    . R.printTextAt
      font
      ( TT.pixelSizeInPointAtDpi
          (unsafeFrom @Int @Float $ unPx size)
          (env ^. #envImg . #imgDpi . #unPx)
      )
      ( R.V2
          ( unsafeFrom @Int @Float
              . unPx
              . maybe offX (Px . (unPx offX +) . unPx)
              $ itemPadX item
          )
          ( unsafeFrom @Int @Float
              $ unPx offN
          )
      )
    $ from @Text @String text
  pure offN
  where
    text = itemData item
    size = itemSize item
    offN =
      Px
        $ unPx offY
        + unPx (fromMaybe (env ^. #envImg . #imgPadY) $ itemPadY item)
        + unPx size

mkQr ::
  Env ->
  Px ->
  Px ->
  Item ->
  R.Drawing CPT.PixelRGBA8 Px
mkQr env offX offY item = do
  R.drawImage img 0
    $ R.V2
      ( unsafeFrom @Int @Float
          . unPx
          . maybe offX (Px . (unPx offX +) . unPx)
          $ itemPadX item
      )
      ( unsafeFrom @Int @Float
          $ unPx offN
      )
  pure
    . Px
    $ unPx offN
    + unPx size
  where
    size = itemSize item
    offN =
      Px
        $ unPx offY
        + unPx (fromMaybe (env ^. #envImg . #imgPadY) $ itemPadY item)
    qr =
      fromMaybe (error "Can not generate qr!")
        . QR.encodeAutomatic
          (QR.defaultQRCodeOptions QR.L)
          QR.Iso8859_1OrUtf8WithoutECI
        $ itemData item
    img =
      CPT.promoteImage
        $ QRJP.toImage 0 (unPx size `div` QR.qrImageSize qr) qr

newtype Px = Px
  { unPx :: Int
  }
  deriving stock
    ( Eq,
      Ord,
      Show,
      Data,
      Generic
    )
  deriving newtype
    ( HasCodec,
      HasItemCodec
    )

data Cfg = Cfg
  { cfgImg :: Img,
    cfgFont :: CfgFont,
    cfgExtra :: [Extra],
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

data Img = Img
  { imgDpi :: Px,
    imgWidth :: Px,
    imgHeight :: Px,
    imgPadX :: Px,
    imgPadY :: Px
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
    via GenericType Img

data CfgFont = CfgFont
  { cfgFontHead :: Text,
    cfgFontIcon :: Text,
    cfgFontNote :: Text
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
    via GenericType CfgFont

data Extra = Extra
  { extraWidth :: Px,
    extraHeight :: Px,
    extraVertical :: Bool
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
    via GenericType Extra

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
    itemSize :: Px,
    itemPadX :: Maybe Px,
    itemPadY :: Maybe Px
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
  { envImg :: Img,
    envFont :: Font,
    envGroup :: [Group]
  }
  deriving stock (Show, Generic)

data Font = Font
  { fontHead :: TT.Font,
    fontIcon :: TT.Font,
    fontNote :: TT.Font
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
