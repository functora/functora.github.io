module App.Widgets.Header
  ( headerEditor,
    headerViewer,
    headerWrapper,
  )
where

import App.Prelude
import App.Types
import qualified App.Widgets.Cell as Cell
import qualified App.Widgets.Field as Field
import qualified Material.LayoutGrid as LayoutGrid
import qualified Material.Typography as Typography
import Miso hiding (at, view)

headerEditor ::
  Model ->
  ATraversal' Model (Field DynamicField Unique) ->
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
        [ Field.field
            st
            optic
            ( opts
                & #optsFilledOrOutlined
                .~ Outlined
            )
            parseDynamicField
            inspectDynamicField
        ]
  ]

headerViewer :: Text -> [View Action]
headerViewer txt =
  if txt == mempty
    then mempty
    else
      [ LayoutGrid.cell
          [ LayoutGrid.span12,
            Typography.headline5,
            style_ [("text-align", "center")]
          ]
          [ text $ ms txt
          ]
      ]

headerWrapper :: [View Action] -> [View Action]
headerWrapper xs =
  if null xs
    then mempty
    else [LayoutGrid.cell [LayoutGrid.span12] xs]
