module App.Widgets.Menu
  ( menu,
  )
where

import App.Types
import qualified App.Widgets.Cell as Cell
import Functora.Prelude as Prelude
import qualified Material.Button as Button
import qualified Material.Dialog as Dialog
import qualified Material.IconButton as IconButton
import qualified Material.Theme as Theme
import qualified Material.TopAppBar as TopAppBar
import Miso hiding (view)

menu :: Model -> [View Action]
menu st =
  [ TopAppBar.shortCollapsed
      TopAppBar.config
      [ TopAppBar.row
          mempty
          [ TopAppBar.section
              [ TopAppBar.alignStart
              ]
              [ IconButton.iconButton
                  ( IconButton.config
                      & IconButton.setOnClick opened
                      & IconButton.setAttributes [TopAppBar.navigationIcon]
                  )
                  "menu"
              ]
          ]
      ],
    Dialog.dialog
      ( Dialog.config
          & Dialog.setOnClose closed
          & Dialog.setOpen (Opened == st ^. #modelMenu)
      )
      ( Dialog.dialogContent
          Nothing
          [ Cell.grid
              mempty
              [ Cell.mediumCell
                  $ Button.raised
                    ( Button.config
                        & Button.setOnClick (screen Converter)
                        & Button.setAttributes
                          [ Theme.secondaryBg,
                            class_ "fill"
                          ]
                    )
                    "Show converter",
                Cell.mediumCell
                  $ Button.raised
                    ( Button.config
                        & Button.setOnClick (screen Editor)
                        & Button.setAttributes
                          [ Theme.secondaryBg,
                            class_ "fill"
                          ]
                    )
                    "Create invoice",
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
    opened = pureUpdate 0 (& #modelMenu .~ Opened)
    closed = pureUpdate 0 (& #modelMenu .~ Closed)
    screen x =
      pureUpdate 0
        $ (#modelMenu .~ Closed)
        . (#modelState . #stScreen .~ x)