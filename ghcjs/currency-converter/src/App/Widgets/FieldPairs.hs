module App.Widgets.FieldPairs
  ( fieldPairs,
  )
where

import qualified App.Misc as Misc
import App.Types
import qualified App.Widgets.Field as Field
import qualified App.Widgets.Header as Header
import qualified App.Widgets.IconToggles as IconToggles
import Functora.Prelude as Prelude
import qualified Material.IconToggle as IconToggle
import Miso hiding (at, view)

fieldPairs ::
  HeaderOrFooter ->
  Model ->
  ATraversal' Model [FieldPair FieldOutput Unique] ->
  [View Action]
fieldPairs hof st optic =
  case hof of
    Header -> (Header.header "Details" (Just (Misc.newFieldPairAction optic)) :)
    Footer -> id
    $ do
      idx <- fst <$> zip [0 ..] (fromMaybe mempty $ st ^? cloneTraversal optic)
      fieldPairWidget st optic idx

fieldPairWidget ::
  Model ->
  ATraversal' Model [FieldPair FieldOutput Unique] ->
  Int ->
  [View Action]
fieldPairWidget st optic idx =
  [ Field.textField
      st
      ( cloneTraversal optic
          . ix idx
          . #fieldPairKey
      )
      ( Field.opts
          & #optsPlaceholder
          .~ ("Label " <> idxTxt)
      ),
    Field.dynamicField
      st
      ( cloneTraversal optic
          . ix idx
          . #fieldPairValue
      )
      ( Field.opts
          & #optsPlaceholder
          .~ ("Value " <> idxTxt)
      ),
    IconToggles.iconToggles
      st
      [ ( "Value " <> idxTxt <> " text",
          IconToggle.icon "font_download",
          IconToggle.icon "font_download_off",
          cloneTraversal optic . ix idx . #fieldPairValuePlainText
        ),
        ( "Value " <> idxTxt <> " QR code",
          IconToggle.icon "qr_code_2",
          IconToggle.icon "developer_board_off",
          cloneTraversal optic . ix idx . #fieldPairValueQrCode
        ),
        ( "Value " <> idxTxt <> " link",
          IconToggle.icon "link",
          IconToggle.icon "link_off",
          cloneTraversal optic . ix idx . #fieldPairValueLink
        ),
        ( "Value " <> idxTxt <> " HTML",
          IconToggle.icon "code",
          IconToggle.icon "code_off",
          cloneTraversal optic . ix idx . #fieldPairValueHtml
        )
      ],
    Header.navHeaderSimple st optic idx
  ]
  where
    idxTxt :: Text
    idxTxt = "#" <> inspect (idx + 1)
