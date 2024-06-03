module App.Widgets.Header
  ( headerEditor,
    header,
  )
where

import App.Types
import qualified App.Widgets.Cell as Cell
import qualified App.Widgets.Field as Field
import Functora.Prelude hiding (Field)
import qualified Material.Fab as Fab
import qualified Material.LayoutGrid as LayoutGrid
import qualified Material.Theme as Theme
import qualified Material.Typography as Typography
import Miso hiding (at, view)
import Miso.String (ms)

headerEditor ::
  Model ->
  ATraversal' Model (Field Text Unique) ->
  Field.Opts ->
  [View Action]
headerEditor st optic opts =
  [ Cell.bigCell
      $ div_
        [ style_
            [ ("display", "flex"),
              ("align-items", "center"),
              ("justify-content", "center")
            ]
        ]
        [ Field.textField st optic
            $ opts
            & #optsFilledOrOutlined
            .~ Outlined
        ]
  ]

header :: Text -> Maybe Action -> View Action
header txt action =
  LayoutGrid.cell
    [ LayoutGrid.span12,
      Typography.headline4,
      style_
        [ ("text-align", "center")
        ]
    ]
    $ text (ms txt)
    : maybe
      mempty
      ( \act ->
          [ text " ",
            Fab.fab
              ( Fab.config
                  & Fab.setOnClick act
                  & Fab.setAttributes [Theme.secondaryBg]
              )
              "add"
          ]
      )
      action
