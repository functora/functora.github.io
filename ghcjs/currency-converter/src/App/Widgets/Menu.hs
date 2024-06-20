module App.Widgets.Menu
  ( menu,
  )
where

import App.Types
import qualified App.Widgets.Button as Button
import qualified App.Widgets.Cell as Cell
import qualified App.Widgets.Modal as Modal
import qualified App.Widgets.Templates as Templates
import Functora.Prelude as Prelude
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
    Modal.modal
      st
      Modal.defOpts
      #modelMenu
      [ Cell.grid
          mempty
          [ Cell.mediumCell
              $ Button.button
                ( Button.defOpts
                    & #optsOnClick
                    .~ screen Converter
                    & #optsLeadingIcon
                    .~ Just "currency_exchange"
                    & #optsExtraAttributes
                    .~ [Theme.secondaryBg]
                    & #optsLabel
                    .~ Just "Converter"
                ),
            Cell.mediumCell
              $ Button.button
                ( Button.defOpts
                    & #optsOnClick
                    .~ screen Editor
                    & #optsLeadingIcon
                    .~ Just "build_circle"
                    & #optsExtraAttributes
                    .~ [Theme.secondaryBg]
                    & #optsLabel
                    .~ Just "Editor"
                ),
            Cell.mediumCell
              $ Button.button
                ( Button.defOpts
                    & #optsOnClick
                    .~ templates #modelTemplates
                    & #optsLeadingIcon
                    .~ Just "apps"
                    & #optsExtraAttributes
                    .~ [Theme.secondaryBg]
                    & #optsLabel
                    .~ Just "Templates"
                ),
            Cell.mediumCell
              $ Button.button
                ( Button.defOpts
                    & #optsOnClick
                    .~ templates #modelExamples
                    & #optsLeadingIcon
                    .~ Just "mood"
                    & #optsExtraAttributes
                    .~ [Theme.secondaryBg]
                    & #optsLabel
                    .~ Just "Examples"
                ),
            Cell.bigCell
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
  ]
    <> Templates.templates #modelTemplates Templates.unfilled Editor st
    <> Templates.templates #modelExamples Templates.examples Viewer st
  where
    opened = pureUpdate 0 (& #modelMenu .~ Opened)
    closed = pureUpdate 0 (& #modelMenu .~ Closed)
    screen x =
      pureUpdate 0
        $ (#modelMenu .~ Closed)
        . (#modelState . #stScreen .~ x)
    templates opt =
      pureUpdate 0
        $ (opt .~ Opened)
        . (#modelMenu .~ Closed)
