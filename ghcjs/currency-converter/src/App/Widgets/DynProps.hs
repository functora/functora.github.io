module App.Widgets.DynProps
  ( dynProps,
  )
where

import qualified App.Misc as Misc
import App.Types
import qualified App.Widgets.DynInput as DynInput
import qualified App.Widgets.Header as Header
import qualified App.Widgets.IconToggles as IconToggles
import qualified App.Widgets.TextInput as TextInput
import Functora.Prelude as Prelude
import qualified Material.IconToggle as IconToggle
import Miso hiding (at, view)

dynProps ::
  HeaderOrFooter ->
  Model ->
  ATraversal' Model [DynProp Unique] ->
  [View Action]
dynProps hof st optic =
  case hof of
    Header -> (Header.header "Details" (Just (Misc.newDynPropAction optic)) :)
    Footer -> id
    $ do
      idx <- fst <$> zip [0 ..] (fromMaybe mempty $ st ^? cloneTraversal optic)
      dynPropWidget st optic idx

dynPropWidget ::
  Model ->
  ATraversal' Model [DynProp Unique] ->
  Int ->
  [View Action]
dynPropWidget st optic idx =
  [ TextInput.textInput
      st
      ( cloneTraversal optic
          . ix idx
          . #dynPropKey
      )
      ( TextInput.opts
          & #optsPlaceholder
          .~ ("Label " <> idxTxt)
      ),
    DynInput.dynInput
      st
      ( cloneTraversal optic
          . ix idx
          . #dynPropValue
      )
      ( DynInput.opts
          & #optsPlaceholder
          .~ ("Value " <> idxTxt)
      ),
    IconToggles.iconToggles
      st
      [ ( "Value " <> idxTxt <> " text",
          IconToggle.icon "font_download",
          IconToggle.icon "font_download_off",
          cloneTraversal optic . ix idx . #dynPropValuePlainText
        ),
        ( "Value " <> idxTxt <> " QR code",
          IconToggle.icon "qr_code_2",
          IconToggle.icon "developer_board_off",
          cloneTraversal optic . ix idx . #dynPropValueQrCode
        ),
        ( "Value " <> idxTxt <> " link",
          IconToggle.icon "link",
          IconToggle.icon "link_off",
          cloneTraversal optic . ix idx . #dynPropValueLink
        ),
        ( "Value " <> idxTxt <> " HTML",
          IconToggle.icon "code",
          IconToggle.icon "code_off",
          cloneTraversal optic . ix idx . #dynPropValueHtml
        )
      ],
    Header.navHeaderSimple st optic idx
  ]
  where
    idxTxt :: Text
    idxTxt = "#" <> inspect (idx + 1)
