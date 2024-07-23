module App.Widgets.Fav
  ( fav,
  )
where

import qualified App.Misc as Misc
import App.Prelude as Prelude
import App.Types
import qualified App.Widgets.Cell as Cell
import qualified App.Widgets.Field as Field
import Lens.Micro (non)
import qualified Material.Button as Button
import qualified Material.Dialog as Dialog
import qualified Material.Theme as Theme
import Miso hiding (at, view)
import qualified Text.URI as URI

fav :: Model -> [View Action]
fav st =
  if st ^. #modelFav == Closed
    then mempty
    else
      [ Dialog.dialog
          ( Dialog.config
              & Dialog.setOnClose closed
              & Dialog.setOpen (Opened == st ^. #modelFav)
          )
          ( Dialog.dialogContent
              Nothing
              [ Cell.grid
                  mempty
                  $ [ Cell.mediumCell
                        $ Field.textField
                          st
                          ( #modelState
                              . #stDoc
                              . #stDocPreFavName
                          )
                          ( Field.defOpts
                              & #optsPlaceholder
                              .~ "Label"
                              & #optsFilledOrOutlined
                              .~ Outlined
                          ),
                      Cell.smallCell
                        $ Button.raised
                          ( Button.config
                              & Button.setOnClick save
                              & Button.setIcon (Just "add_box")
                              & Button.setAttributes
                                [ Theme.secondaryBg,
                                  class_ "fill"
                                ]
                          )
                          "Save",
                      Cell.smallCell
                        $ Button.raised
                          ( Button.config
                              & Button.setOnClick Noop
                              & Button.setIcon (Just "delete_forever")
                              & Button.setAttributes
                                [ Theme.secondaryBg,
                                  class_ "fill"
                                ]
                          )
                          "Delete",
                      Cell.bigCell
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
    closed = pureUpdate 0 (& #modelFav .~ Closed)
    fullFavName = makeFavName st
    save = PushUpdate $ do
      ct <- getCurrentTime
      uri <- URI.mkURI $ shareLink (st ^. #modelState . #stScreen) st
      let nextFav =
            Fav
              { favUri = uri,
                favCreatedAt = ct
              }
      pure
        . ChanItem 0
        $ (Misc.textPopupPure $ "Saved " <> fullFavName <> "!")
        . (& #modelFavMap . at fullFavName . non nextFav . #favUri .~ uri)

makeFavName :: Model -> Text
makeFavName st =
  preFavName
    <> ( if preFavName == mempty
          then mempty
          else " "
       )
    <> getCode #stConvTopMoney
    <> "/"
    <> getCode #stConvBottomMoney
  where
    preFavName =
      st
        ^. #modelState
        . #stDoc
        . #stDocPreFavName
        . #fieldOutput
    getCode optic =
      st
        ^. #modelState
        . #stDoc
        . #stDocConv
        . cloneLens optic
        . #moneyCurrency
        . #currencyOutput
        . #currencyInfoCode
        . #unCurrencyCode
        . to toMisoString
