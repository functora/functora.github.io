{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Codec.Xlsx.Types.RichText
  ( -- * Main types
    RichTextRun (..),
    RunProperties (..),
    applyRunProperties,
  )
where

import GHC.Generics (Generic)

#ifdef USE_MICROLENS
import Lens.Micro.TH (makeLenses)
#else
import Control.Lens hiding (element)
#endif
import Codec.Xlsx.Parser.Internal
import Codec.Xlsx.Types.StyleSheet
import Codec.Xlsx.Writer.Internal
import Control.DeepSeq (NFData)
import Control.Monad
import Data.Default
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Text.XML
import Text.XML.Cursor

-- | Rich Text Run
--
-- This element represents a run of rich text. A rich text run is a region of
-- text that share a common set of properties, such as formatting properties.
--
-- Section 18.4.4, "r (Rich Text Run)" (p. 1724)
data RichTextRun = RichTextRun
  { -- | This element represents a set of properties to apply to the contents of
    -- this rich text run.
    richTextRunProperties :: Maybe RunProperties,
    -- | This element represents the text content shown as part of a string.
    --
    -- NOTE: 'RichTextRun' elements with an empty text field will result in
    -- an error when opening the file in Excel.
    --
    -- Section 18.4.12, "t (Text)" (p. 1727)
    richTextRunText :: Text
  }
  deriving (Eq, Ord, Show, Generic)

instance NFData RichTextRun

-- | Run properties
--
-- Section 18.4.7, "rPr (Run Properties)" (p. 1725)
data RunProperties = RunProperties
  { -- | Displays characters in bold face font style.
    --
    -- Section 18.8.2, "b (Bold)" (p. 1757)
    runPropertiesBold :: Maybe Bool,
    -- | This element defines the font character set of this font.
    --
    -- Section 18.4.1, "charset (Character Set)" (p. 1721)
    runPropertiesCharset :: Maybe Int,
    -- | One of the colors associated with the data bar or color scale.
    --
    -- Section 18.3.1.15, "color (Data Bar Color)" (p. 1608)
    runPropertiesColor :: Maybe Color,
    -- | Macintosh compatibility setting. Represents special word/character
    -- rendering on Macintosh, when this flag is set. The effect is to condense
    -- the text (squeeze it together).
    --
    -- Section 18.8.12, "condense (Condense)" (p. 1764)
    runPropertiesCondense :: Maybe Bool,
    -- | This element specifies a compatibility setting used for previous
    -- spreadsheet applications, resulting in special word/character rendering
    -- on those legacy applications, when this flag is set. The effect extends
    -- or stretches out the text.
    --
    -- Section 18.8.17, "extend (Extend)" (p. 1766)
    runPropertiesExtend :: Maybe Bool,
    -- | The font family this font belongs to. A font family is a set of fonts
    -- having common stroke width and serif characteristics. This is system
    -- level font information. The font name overrides when there are
    -- conflicting values.
    --
    -- Section 18.8.18, "family (Font Family)" (p. 1766)
    runPropertiesFontFamily :: Maybe FontFamily,
    -- | Displays characters in italic font style. The italic style is defined
    -- by the font at a system level and is not specified by ECMA-376.
    --
    -- Section 18.8.26, "i (Italic)" (p. 1773)
    runPropertiesItalic :: Maybe Bool,
    -- | This element displays only the inner and outer borders of each
    -- character. This is very similar to Bold in behavior.
    --
    -- Section 18.4.2, "outline (Outline)" (p. 1722)
    runPropertiesOutline :: Maybe Bool,
    -- | This element is a string representing the name of the font assigned to
    -- display this run.
    --
    -- Section 18.4.5, "rFont (Font)" (p. 1724)
    runPropertiesFont :: Maybe Text,
    -- | Defines the font scheme, if any, to which this font belongs. When a
    -- font definition is part of a theme definition, then the font is
    -- categorized as either a major or minor font scheme component. When a new
    -- theme is chosen, every font that is part of a theme definition is updated
    -- to use the new major or minor font definition for that theme. Usually
    -- major fonts are used for styles like headings, and minor fonts are used
    -- for body and paragraph text.
    --
    -- Section 18.8.35, "scheme (Scheme)" (p. 1794)
    runPropertiesScheme :: Maybe FontScheme,
    -- | Macintosh compatibility setting. Represents special word/character
    -- rendering on Macintosh, when this flag is set. The effect is to render a
    -- shadow behind, beneath and to the right of the text.
    --
    -- Section 18.8.36, "shadow (Shadow)" (p. 1795)
    runPropertiesShadow :: Maybe Bool,
    -- | This element draws a strikethrough line through the horizontal middle
    -- of the text.
    --
    -- Section 18.4.10, "strike (Strike Through)" (p. 1726)
    runPropertiesStrikeThrough :: Maybe Bool,
    -- | This element represents the point size (1/72 of an inch) of the Latin
    -- and East Asian text.
    --
    -- Section 18.4.11, "sz (Font Size)" (p. 1727)
    runPropertiesSize :: Maybe Double,
    -- | This element represents the underline formatting style.
    --
    -- Section 18.4.13, "u (Underline)" (p. 1728)
    runPropertiesUnderline :: Maybe FontUnderline,
    -- | This element adjusts the vertical position of the text relative to the
    -- text's default appearance for this run. It is used to get 'superscript'
    -- or 'subscript' texts, and shall reduce the font size (if a smaller size
    -- is available) accordingly.
    --
    -- Section 18.4.14, "vertAlign (Vertical Alignment)" (p. 1728)
    runPropertiesVertAlign :: Maybe FontVerticalAlignment
  }
  deriving (Eq, Ord, Show, Generic)

instance NFData RunProperties

{-------------------------------------------------------------------------------
  Default instances
-------------------------------------------------------------------------------}

instance Default RichTextRun where
  def =
    RichTextRun
      { richTextRunProperties = Nothing,
        richTextRunText = ""
      }

instance Default RunProperties where
  def =
    RunProperties
      { runPropertiesBold = Nothing,
        runPropertiesCharset = Nothing,
        runPropertiesColor = Nothing,
        runPropertiesCondense = Nothing,
        runPropertiesExtend = Nothing,
        runPropertiesFontFamily = Nothing,
        runPropertiesItalic = Nothing,
        runPropertiesOutline = Nothing,
        runPropertiesFont = Nothing,
        runPropertiesScheme = Nothing,
        runPropertiesShadow = Nothing,
        runPropertiesStrikeThrough = Nothing,
        runPropertiesSize = Nothing,
        runPropertiesUnderline = Nothing,
        runPropertiesVertAlign = Nothing
      }

{-------------------------------------------------------------------------------
  Rendering
-------------------------------------------------------------------------------}

-- | See @CT_RElt@, p. 3903
instance ToElement RichTextRun where
  toElement nm RichTextRun {..} =
    Element
      { elementName = nm,
        elementAttributes = Map.empty,
        elementNodes =
          map NodeElement . catMaybes $
            [ toElement "rPr" <$> richTextRunProperties,
              Just $ elementContentPreserved "t" richTextRunText
            ]
      }

-- | See @CT_RPrElt@, p. 3903
instance ToElement RunProperties where
  toElement nm RunProperties {..} =
    Element
      { elementName = nm,
        elementAttributes = Map.empty,
        elementNodes =
          map NodeElement . catMaybes $
            [ elementValue "rFont" <$> runPropertiesFont,
              elementValue "charset" <$> runPropertiesCharset,
              elementValue "family" <$> runPropertiesFontFamily,
              elementValue "b" <$> runPropertiesBold,
              elementValue "i" <$> runPropertiesItalic,
              elementValue "strike" <$> runPropertiesStrikeThrough,
              elementValue "outline" <$> runPropertiesOutline,
              elementValue "shadow" <$> runPropertiesShadow,
              elementValue "condense" <$> runPropertiesCondense,
              elementValue "extend" <$> runPropertiesExtend,
              toElement "color" <$> runPropertiesColor,
              elementValue "sz" <$> runPropertiesSize,
              elementValueDef "u" FontUnderlineSingle
                <$> runPropertiesUnderline,
              elementValue "vertAlign" <$> runPropertiesVertAlign,
              elementValue "scheme" <$> runPropertiesScheme
            ]
      }

{-------------------------------------------------------------------------------
  Parsing
-------------------------------------------------------------------------------}

-- | See @CT_RElt@, p. 3903
instance FromCursor RichTextRun where
  fromCursor cur = do
    richTextRunText <- cur $/ element (n_ "t") &/ content
    richTextRunProperties <- maybeFromElement (n_ "rPr") cur
    return RichTextRun {..}

instance FromXenoNode RichTextRun where
  fromXenoNode root = do
    (prNode, tNode) <-
      collectChildren root $ (,) <$> maybeChild "rPr" <*> requireChild "t"
    richTextRunProperties <- mapM fromXenoNode prNode
    richTextRunText <- contentX tNode
    return RichTextRun {..}

-- | See @CT_RPrElt@, p. 3903
instance FromCursor RunProperties where
  fromCursor cur = do
    runPropertiesFont <- maybeElementValue (n_ "rFont") cur
    runPropertiesCharset <- maybeElementValue (n_ "charset") cur
    runPropertiesFontFamily <- maybeElementValue (n_ "family") cur
    runPropertiesBold <- maybeBoolElementValue (n_ "b") cur
    runPropertiesItalic <- maybeBoolElementValue (n_ "i") cur
    runPropertiesStrikeThrough <- maybeBoolElementValue (n_ "strike") cur
    runPropertiesOutline <- maybeBoolElementValue (n_ "outline") cur
    runPropertiesShadow <- maybeBoolElementValue (n_ "shadow") cur
    runPropertiesCondense <- maybeBoolElementValue (n_ "condense") cur
    runPropertiesExtend <- maybeBoolElementValue (n_ "extend") cur
    runPropertiesColor <- maybeFromElement (n_ "color") cur
    runPropertiesSize <- maybeElementValue (n_ "sz") cur
    runPropertiesUnderline <-
      maybeElementValueDef (n_ "u") FontUnderlineSingle cur
    runPropertiesVertAlign <- maybeElementValue (n_ "vertAlign") cur
    runPropertiesScheme <- maybeElementValue (n_ "scheme") cur
    return RunProperties {..}

instance FromXenoNode RunProperties where
  fromXenoNode root = collectChildren root $ do
    runPropertiesFont <- maybeElementVal "rFont"
    runPropertiesCharset <- maybeElementVal "charset"
    runPropertiesFontFamily <- maybeElementVal "family"
    runPropertiesBold <- maybeElementVal "b"
    runPropertiesItalic <- maybeElementVal "i"
    runPropertiesStrikeThrough <- maybeElementVal "strike"
    runPropertiesOutline <- maybeElementVal "outline"
    runPropertiesShadow <- maybeElementVal "shadow"
    runPropertiesCondense <- maybeElementVal "condense"
    runPropertiesExtend <- maybeElementVal "extend"
    runPropertiesColor <- maybeFromChild "color"
    runPropertiesSize <- maybeElementVal "sz"
    runPropertiesUnderline <- maybeElementVal "u"
    runPropertiesVertAlign <- maybeElementVal "vertAlign"
    runPropertiesScheme <- maybeElementVal "scheme"
    return RunProperties {..}

{-------------------------------------------------------------------------------
  Applying formatting
-------------------------------------------------------------------------------}

#if (MIN_VERSION_base(4,11,0))
instance Semigroup RunProperties where
  a <> b = RunProperties {
      runPropertiesBold          = override runPropertiesBold
    , runPropertiesCharset       = override runPropertiesCharset
    , runPropertiesColor         = override runPropertiesColor
    , runPropertiesCondense      = override runPropertiesCondense
    , runPropertiesExtend        = override runPropertiesExtend
    , runPropertiesFontFamily    = override runPropertiesFontFamily
    , runPropertiesItalic        = override runPropertiesItalic
    , runPropertiesOutline       = override runPropertiesOutline
    , runPropertiesFont          = override runPropertiesFont
    , runPropertiesScheme        = override runPropertiesScheme
    , runPropertiesShadow        = override runPropertiesShadow
    , runPropertiesStrikeThrough = override runPropertiesStrikeThrough
    , runPropertiesSize          = override runPropertiesSize
    , runPropertiesUnderline     = override runPropertiesUnderline
    , runPropertiesVertAlign     = override runPropertiesVertAlign
    }
    where
      override :: (RunProperties -> Maybe x) -> Maybe x
      override f = f b `mplus` f a

#endif

-- | The 'Monoid' instance for 'RunProperties' is biased: later properties
-- override earlier ones.
instance Monoid RunProperties where
  mempty = def
  a `mappend` b =
    RunProperties
      { runPropertiesBold = override runPropertiesBold,
        runPropertiesCharset = override runPropertiesCharset,
        runPropertiesColor = override runPropertiesColor,
        runPropertiesCondense = override runPropertiesCondense,
        runPropertiesExtend = override runPropertiesExtend,
        runPropertiesFontFamily = override runPropertiesFontFamily,
        runPropertiesItalic = override runPropertiesItalic,
        runPropertiesOutline = override runPropertiesOutline,
        runPropertiesFont = override runPropertiesFont,
        runPropertiesScheme = override runPropertiesScheme,
        runPropertiesShadow = override runPropertiesShadow,
        runPropertiesStrikeThrough = override runPropertiesStrikeThrough,
        runPropertiesSize = override runPropertiesSize,
        runPropertiesUnderline = override runPropertiesUnderline,
        runPropertiesVertAlign = override runPropertiesVertAlign
      }
    where
      override :: (RunProperties -> Maybe x) -> Maybe x
      override f = f b `mplus` f a

-- | Apply properties to a 'RichTextRun'
--
-- If the 'RichTextRun' specifies its own properties, then these overrule the
-- properties specified here. For example, adding @bold@ to a 'RichTextRun'
-- which is already @italic@ will make the 'RichTextRun' both @bold and @italic@
-- but adding it to one that that is explicitly _not_ bold will leave the
-- 'RichTextRun' unchanged.
applyRunProperties :: RunProperties -> RichTextRun -> RichTextRun
applyRunProperties p (RichTextRun Nothing t) = RichTextRun (Just p) t
applyRunProperties p (RichTextRun (Just p') t) = RichTextRun (Just (p `mappend` p')) t
