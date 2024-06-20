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
                ( Button.defOpts @Action
                    & #optsLabel
                    .~ Just @Text "Converter"
                    & #optsLeadingIcon
                    .~ Just @Text "currency_exchange"
                    & #optsExtraAttributes
                    .~ [Theme.secondaryBg]
                    & (#optsOnClick :: Lens' (Button.Opts Action) (Maybe Action))
                    .~ Just (screen Converter)
                ),
            Cell.mediumCell
              $ Button.button
                ( Button.defOpts @Action
                    & #optsLabel
                    .~ Just @Text "Editor"
                    & #optsLeadingIcon
                    .~ Just @Text "build_circle"
                    & #optsExtraAttributes
                    .~ [Theme.secondaryBg]
                    & (#optsOnClick :: Lens' (Button.Opts Action) (Maybe Action))
                    .~ Just (screen Editor)
                ),
            Cell.mediumCell
              $ Button.button
                ( Button.defOpts @Action
                    & #optsLabel
                    .~ Just @Text "Templates"
                    & #optsLeadingIcon
                    .~ Just @Text "apps"
                    & #optsExtraAttributes
                    .~ [Theme.secondaryBg]
                    & (#optsOnClick :: Lens' (Button.Opts Action) (Maybe Action))
                    .~ Just (templates #modelTemplates)
                ),
            Cell.mediumCell
              $ Button.button
                ( Button.defOpts @Action
                    & #optsLabel
                    .~ Just @Text "Examples"
                    & #optsLeadingIcon
                    .~ Just @Text "mood"
                    & #optsExtraAttributes
                    .~ [Theme.secondaryBg]
                    & (#optsOnClick :: Lens' (Button.Opts Action) (Maybe Action))
                    .~ Just (templates #modelExamples)
                ),
            Cell.bigCell
              $ Button.button
                ( Button.defOpts @Action
                    & #optsLabel
                    .~ Just @Text "Back"
                    & #optsLeadingIcon
                    .~ Just @Text "arrow_back"
                    & (#optsOnClick :: Lens' (Button.Opts Action) (Maybe Action))
                    .~ Just closed
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
