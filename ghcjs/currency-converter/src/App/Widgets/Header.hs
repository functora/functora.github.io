module App.Widgets.Header
  ( headerEditor,
    headerViewer,
    headerWrapper,
  )
where

import App.Types
import qualified App.Widgets.Field as Field
import Functora.Miso.Prelude
import qualified Functora.Miso.Widgets.Grid as Grid
import qualified Material.LayoutGrid as LayoutGrid
import qualified Material.Typography as Typography

headerEditor ::
  Model ->
  ATraversal' Model (Field DynamicField Unique) ->
  Field.Opts ->
  [View Action]
headerEditor st optic opts =
  [ Grid.bigCell
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

headerViewer :: MisoString -> [View Action]
headerViewer txt =
  if txt == mempty
    then mempty
    else
      [ LayoutGrid.cell
          [ LayoutGrid.span12,
            Typography.headline5,
            style_ [("text-align", "center")]
          ]
          [ text txt
          ]
      ]

headerWrapper :: [View Action] -> [View Action]
headerWrapper xs =
  if null xs
    then mempty
    else [LayoutGrid.cell [LayoutGrid.span12] xs]
