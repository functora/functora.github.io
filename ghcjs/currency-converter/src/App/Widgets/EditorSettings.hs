module App.Widgets.EditorSettings
  ( editorSettings,
  )
where

import qualified App.Misc as Misc
import App.Types
import qualified App.Widgets.Button as Button
import qualified App.Widgets.Cell as Cell
import qualified App.Widgets.Field as Field
import qualified App.Widgets.Header as Header
import qualified App.Widgets.Modal as Modal
import App.Widgets.Templates
import Functora.Prelude as Prelude
import qualified Material.Select as Select
import qualified Material.Select.Item as SelectItem
import qualified Material.TextArea as TextArea
import qualified Material.Theme as Theme
import Miso hiding (view)
import Miso.String (ms)
import qualified Text.URI as URI

editorSettings :: Model -> [View Action]
editorSettings st =
  Header.headerEditor
    st
    ( #modelState
        . #stPre
    )
    ( Field.defOpts
        & #optsPlaceholder
        .~ ( "Preview - "
              <> ( st
                    ^. #modelState
                    . #stPre
                    . #fieldType
                    . to userFieldType
                 )
           )
        & #optsLeadingWidget
        .~ Just
          ( Field.ModalWidget
              $ Field.ModalMiniWidget (#modelState . #stPre)
          )
    )
    <> [ shareModal st,
         Cell.mediumCell
          $ Field.passwordField st (#modelState . #stIkm) Field.defOpts,
         Cell.mediumCell
          $ selectLayoutWidget st,
         Cell.mediumCell
          $ Button.button
            ( Button.defOpts
                & #optsLeadingIcon
                .~ Just "share"
                & #optsOnClick
                .~ ( Misc.copyIntoClipboardAction st
                      $ shareLink @Text Viewer st
                   )
                & #optsLabel
                .~ Just "Link"
            ),
         Cell.mediumCell
          $ Button.button
            ( Button.defOpts
                & #optsLeadingIcon
                .~ Just "more_horiz"
                & #optsOnClick
                .~ pureUpdate 0 (& #modelShare .~ Opened)
                & #optsLabel
                .~ Just "More"
            )
       ]

selectLayoutWidget :: Model -> View Action
selectLayoutWidget st =
  Select.outlined
    ( Select.config
        & Select.setLabel (Just "Layout")
        & Select.setAttributes [class_ "fill-inner"]
        & Select.setSelected (Just $ st ^. cloneLens optic)
        & Select.setOnChange (\x -> pureUpdate 0 (& cloneLens optic .~ x))
    )
    ( SelectItem.selectItem
        (SelectItem.config item)
        [text . ms $ userMsg item]
    )
    $ fmap
      ( \t ->
          SelectItem.selectItem
            (SelectItem.config t)
            [text . ms $ userMsg t]
      )
      items
  where
    item :| items = enumerateNE @AssetsAndPaymentsLayout
    optic :: ALens' Model AssetsAndPaymentsLayout
    optic = #modelState . #stDoc . #stDocAssetsAndPaymentsLayout
    userMsg :: AssetsAndPaymentsLayout -> Text
    userMsg = \case
      AssetsBeforePayments -> "Assets before payments"
      PaymentsBeforeAssets -> "Payments before assets"

shareModal :: Model -> View Action
shareModal st =
  Modal.modal
    st
    Modal.defOpts
    #modelShare
    [ Cell.grid
        mempty
        $ shareWidget st Viewer
        <> shareWidget st Editor
        <> [ Cell.bigCell
              $ Button.button
                ( Button.defOpts
                    & #optsOnClick
                    .~ closed
                    & #optsLeadingIcon
                    .~ Just "arrow_back"
                    & #optsLabel
                    .~ Just "Back"
                )
           ]
    ]
  where
    closed = pureUpdate 0 (& #modelShare .~ Closed)

shareWidget :: Model -> Screen -> [View Action]
shareWidget st screen =
  [ Cell.bigCell
      . TextArea.filled
      $ TextArea.config
      & TextArea.setValue (Just screenLink)
      & TextArea.setLabel (Just $ inspect screen <> " link")
      & TextArea.setDisabled True
      & TextArea.setFullwidth True,
    Cell.mediumCell
      $ Button.button
        ( Button.defOpts
            & #optsLeadingIcon
            .~ Just "share"
            & #optsExtraAttributes
            .~ [Theme.secondaryBg]
            & #optsOnClick
            .~ Misc.copyIntoClipboardAction st screenLink
            & #optsLabel
            .~ Just (inspect screen)
        ),
    Cell.mediumCell
      $ Button.button
        ( Button.defOpts
            & #optsLeadingIcon
            .~ Just "login"
            & #optsExtraAttributes
            .~ [Theme.secondaryBg]
            & #optsOnClick
            .~ ( PushUpdate $ do
                  uri <- URI.mkURI $ from @String @Text screenLink
                  new <- newModel (Just $ st ^. #modelMarket) uri
                  pure . ChanItem 0 $ const new
               )
            & #optsLabel
            .~ Just (inspect screen)
        ),
    Cell.bigCell
      . TextArea.filled
      $ TextArea.config
      & TextArea.setValue (Just screenQrCode)
      & TextArea.setLabel (Just $ inspect screen <> " QR code")
      & TextArea.setDisabled True
      & TextArea.setFullwidth True,
    Cell.mediumCell
      $ Button.button
        ( Button.defOpts
            & #optsLeadingIcon
            .~ Just "share"
            & #optsExtraAttributes
            .~ [Theme.secondaryBg]
            & #optsOnClick
            .~ Misc.copyIntoClipboardAction st screenQrCode
            & #optsLabel
            .~ Just (inspect screen <> " QR")
        ),
    Cell.mediumCell
      $ Button.button
        ( Button.defOpts
            & #optsLeadingIcon
            .~ Just "login"
            & #optsExtraAttributes
            .~ [Theme.secondaryBg]
            & #optsOnClick
            .~ ( PushUpdate $ do
                  uri <- URI.mkURI $ from @String @Text screenQrCode
                  new <- newModel (Just $ st ^. #modelMarket) uri
                  pure . ChanItem 0 $ const new
               )
            & #optsLabel
            .~ Just (inspect screen <> " QR")
        )
  ]
  where
    screenLink = shareLink screen st
    screenQrCode = shareLink (QrCode screen) st
