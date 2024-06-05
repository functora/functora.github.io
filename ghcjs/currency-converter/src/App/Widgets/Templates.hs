module App.Widgets.Templates
  ( templates,
    unfilled,
    examples,
  )
where

import App.Types
import qualified App.Widgets.Cell as Cell
import Functora.Prelude hiding (Field)
import qualified Material.Button as Button
import qualified Material.Dialog as Dialog
import qualified Material.Theme as Theme
import Miso hiding (at, view)

data Template = Template
  { templateName :: Text,
    templateMaker :: IO (StDoc Unique)
  }
  deriving stock (Generic)

templates ::
  ALens' Model OpenedOrClosed ->
  [Template] ->
  Screen ->
  Model ->
  [View Action]
templates optic tpls sc st =
  [ Dialog.dialog
      ( Dialog.config
          & Dialog.setOnClose closed
          & Dialog.setOpen (Opened == st ^. cloneLens optic)
      )
      ( Dialog.dialogContent
          Nothing
          [ Cell.grid mempty
              $ ( do
                    tpl <- tpls
                    pure
                      . Cell.mediumCell
                      $ Button.raised
                        ( Button.config
                            & Button.setOnClick (screen tpl)
                            & Button.setAttributes
                              [ Theme.secondaryBg,
                                class_ "fill"
                              ]
                        )
                        ( from @Text @String $ tpl ^. #templateName
                        )
                )
              <> [ Cell.bigCell
                    $ Button.raised
                      ( Button.config
                          & Button.setOnClick closed
                          & Button.setIcon (Just "arrow_back")
                          & Button.setAttributes [class_ "fill"]
                      )
                      "Back"
                 ]
          ]
          mempty
      )
  ]
  where
    closed = pureUpdate 0 (& cloneLens optic .~ Closed)
    screen tpl =
      PushUpdate $ do
        doc <- liftIO $ tpl ^. #templateMaker
        pre <- newDynamicTitleField mempty
        pure
          $ ChanItem 0
          $ (cloneLens optic .~ Closed)
          . (#modelState . #stScreen .~ sc)
          . (#modelState . #stDoc .~ doc)
          . (#modelState . #stPre .~ pre)
          . (#modelState . #stExt .~ Nothing)

--
-- Templates
--

unfilled :: [Template]
unfilled =
  [ Template "Empty" emptyTemplate,
    Template "Text" plainTemplate
  ]

emptyTemplate :: IO (StDoc Unique)
emptyTemplate = do
  fhead <- newDynamicTitleField mempty
  ahead <- newDynamicTitleField mempty
  phead <- newDynamicTitleField mempty
  pure
    StDoc
      { stDocFieldPairs = mempty,
        stDocAssets = mempty,
        stDocPaymentMethods = mempty,
        stDocFieldPairsHeader = fhead,
        stDocAssetsHeader = ahead,
        stDocPaymentMethodsHeader = phead
      }

plainTemplate :: IO (StDoc Unique)
plainTemplate = do
  fhead <- newDynamicTitleField mempty
  ahead <- newDynamicTitleField mempty
  phead <- newDynamicTitleField mempty
  pure
    StDoc
      { stDocFieldPairs = mempty,
        stDocAssets = mempty,
        stDocPaymentMethods = mempty,
        stDocFieldPairsHeader = fhead,
        stDocAssetsHeader = ahead,
        stDocPaymentMethodsHeader = phead
      }

--
-- Examples
--

examples :: [Template]
examples =
  [ Template "Empty" emptyTemplate,
    Template "Text" plainExample
  ]

plainExample :: IO (StDoc Unique)
plainExample = do
  fhead <- newDynamicTitleField mempty
  ahead <- newDynamicTitleField mempty
  phead <- newDynamicTitleField mempty
  pure
    StDoc
      { stDocFieldPairs = mempty,
        stDocAssets = mempty,
        stDocPaymentMethods = mempty,
        stDocFieldPairsHeader = fhead,
        stDocAssetsHeader = ahead,
        stDocPaymentMethodsHeader = phead
      }
