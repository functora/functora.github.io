module App.Widgets.EditorSettings
  ( editorSettings,
  )
where

import qualified App.Misc as Misc
import App.Types
import qualified App.Widgets.Button as Button
import qualified App.Widgets.Field as Field
import qualified App.Widgets.Grid as Grid
import qualified App.Widgets.Header as Header
import qualified App.Widgets.Modal as Modal
import qualified App.Widgets.Select as Select
import App.Widgets.Templates
import Functora.Prelude as Prelude
import Miso hiding (view)
import Miso.String (ms)
import qualified Text.URI as URI
import qualified Prelude

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
         Grid.mediumCell
          [ Field.passwordField st (#modelState . #stIkm) Field.defOpts
          ],
         Grid.mediumCell
          [ selectLayoutWidget st
          ],
         Grid.mediumCell
          [ Button.button
              ( Button.defOpts
                  & #optsLabel
                  .~ Just @Text "Link"
                  & #optsLeadingIcon
                  .~ Just FaShare
                  & (#optsOnClick :: Lens' (Button.Opts Action) (Maybe Action))
                  .~ Just
                    ( Misc.copyIntoClipboardAction st
                        $ shareLink @Text Viewer st
                    )
              )
          ],
         Grid.mediumCell
          [ Button.button
              ( Button.defOpts
                  & #optsLabel
                  .~ Just @Text "More"
                  & #optsLeadingIcon
                  .~ Just FaEllipsis
                  & (#optsOnClick :: Lens' (Button.Opts Action) (Maybe Action))
                  .~ Just (pureUpdate 0 (& #modelShare .~ Opened))
              )
          ]
       ]

selectLayoutWidget :: Model -> View Action
selectLayoutWidget st =
  Select.select @Action
    $ Select.defOpts
    & #optsLabel
    .~ Just @Text "Layout"
    & (#optsOnChange :: Lens' (Select.Opts Action) (Maybe (Text -> Action)))
    .~ Just
      ( \x ->
          pureUpdate
            0
            ( &
                cloneLens optic
                  .~ toEnum
                    ( Prelude.read
                        $ from @Text @String x
                    )
            )
      )
    & #optsSelected
    .~ Just (newItem $ st ^. cloneLens optic)
    & (#optsItems :: Lens' (Select.Opts Action) [Select.Item])
    .~ ( fmap newItem
          $ enumerate @AssetsAndPaymentsLayout
       )
  where
    optic :: ALens' Model AssetsAndPaymentsLayout
    optic = #modelState . #stDoc . #stDocAssetsAndPaymentsLayout
    userMsg :: AssetsAndPaymentsLayout -> Text
    userMsg = \case
      AssetsBeforePayments -> "Assets before payments"
      PaymentsBeforeAssets -> "Payments before assets"
    newItem :: AssetsAndPaymentsLayout -> Select.Item
    newItem x =
      Select.Item
        { Select.itemLabel = userMsg x,
          Select.itemLeadingIcon = Nothing,
          Select.itemValue = inspect $ fromEnum x
        }

shareModal :: Model -> View Action
shareModal st =
  Modal.modal
    st
    Modal.defOpts
    #modelShare
    [ Grid.grid
        mempty
        $ shareWidget st Viewer
        <> shareWidget st Editor
        <> [ Grid.bigCell
              [ Button.button
                  ( Button.defOpts
                      & #optsLabel
                      .~ Just @Text "Back"
                      & #optsLeadingIcon
                      .~ Just FaArrowLeft
                      & (#optsOnClick :: Lens' (Button.Opts Action) (Maybe Action))
                      .~ Just closed
                  )
              ]
           ]
    ]
  where
    closed = pureUpdate 0 (& #modelShare .~ Closed)

shareWidget :: Model -> Screen -> [View Action]
shareWidget st screen =
  [ Grid.bigCell
      [ textarea_
          [ value_ $ ms screenLink,
            placeholder_ $ inspect screen <> " link",
            disabled_ True
          ]
          mempty
      ],
    Grid.mediumCell
      [ Button.button
          ( Button.defOpts @Action
              & #optsLabel
              .~ Just @Text (inspect screen)
              & #optsLeadingIcon
              .~ Just FaShare
              & (#optsOnClick :: Lens' (Button.Opts Action) (Maybe Action))
              .~ Just (Misc.copyIntoClipboardAction st screenLink)
          )
      ],
    Grid.mediumCell
      [ Button.button
          ( Button.defOpts @Action
              & #optsLabel
              .~ Just @Text (inspect screen)
              & #optsLeadingIcon
              .~ Just FaArrowRightToBracket
              & (#optsOnClick :: Lens' (Button.Opts Action) (Maybe Action))
              .~ ( Just . PushUpdate $ do
                    uri <- URI.mkURI $ from @String @Text screenLink
                    new <- newModel (Just $ st ^. #modelMarket) uri
                    pure . ChanItem 0 $ const new
                 )
          )
      ],
    Grid.bigCell
      [ textarea_
          [ value_ $ ms screenQrCode,
            placeholder_ $ inspect screen <> " QR code",
            disabled_ True
          ]
          mempty
      ],
    Grid.mediumCell
      [ Button.button
          ( Button.defOpts @Action
              & #optsLabel
              .~ Just @Text (inspect screen <> " QR")
              & #optsLeadingIcon
              .~ Just FaShare
              & (#optsOnClick :: Lens' (Button.Opts Action) (Maybe Action))
              .~ Just (Misc.copyIntoClipboardAction st screenQrCode)
          )
      ],
    Grid.mediumCell
      [ Button.button
          ( Button.defOpts @Action
              & #optsLabel
              .~ Just @Text (inspect screen <> " QR")
              & #optsLeadingIcon
              .~ Just FaArrowRightToBracket
              & (#optsOnClick :: Lens' (Button.Opts Action) (Maybe Action))
              .~ ( Just . PushUpdate $ do
                    uri <- URI.mkURI $ from @String @Text screenQrCode
                    new <- newModel (Just $ st ^. #modelMarket) uri
                    pure . ChanItem 0 $ const new
                 )
          )
      ]
  ]
  where
    screenLink = shareLink screen st
    screenQrCode = shareLink (QrCode screen) st
