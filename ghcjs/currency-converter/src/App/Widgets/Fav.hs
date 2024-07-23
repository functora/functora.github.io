module App.Widgets.Fav
  ( fav,
  )
where

import App.Prelude as Prelude
import App.Types
import qualified App.Widgets.Cell as Cell
import qualified App.Widgets.Field as Field
import qualified Material.Button as Button
import qualified Material.Dialog as Dialog
import qualified Material.Theme as Theme
import Miso hiding (view)

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
                        $ Field.field
                          st
                          ( #modelState . #stPre
                          )
                          ( Field.defOpts
                              & #optsPlaceholder
                              .~ ( "Name - "
                                    <> ( st
                                          ^. #modelState
                                          . #stDoc
                                          . #stDocFavName
                                          . #fieldType
                                          . to userFieldType
                                       )
                                 )
                              & #optsLeadingWidget
                              .~ Just
                                ( Field.ModalWidget
                                    $ Field.ModalMiniWidget
                                      ( #modelState
                                          . #stDoc
                                          . #stDocFavName
                                      )
                                )
                              & #optsFilledOrOutlined
                              .~ Outlined
                          )
                          parseDynamicField
                          inspectDynamicField,
                      Cell.mediumCell
                        $ Button.raised
                          ( Button.config
                              & Button.setOnClick Noop
                              & Button.setIcon (Just "add_box")
                              & Button.setAttributes
                                [ Theme.secondaryBg,
                                  class_ "fill"
                                ]
                          )
                          "Add favorite",
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
