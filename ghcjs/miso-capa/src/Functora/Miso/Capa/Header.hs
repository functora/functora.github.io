module Functora.Miso.Capa.Header
  ( headerEditor,
    headerViewer,
    headerWrapper,
  )
where

import qualified Functora.Miso.Capa.Field as Field
import qualified Functora.Miso.Capa.Grid as Grid
import Functora.Miso.Prelude
import Functora.Miso.Types
import qualified Material.LayoutGrid as LayoutGrid
import qualified Material.Typography as Typography

headerEditor ::
  Field.Args model action DynamicField Unique ->
  Field.Opts model action ->
  [View action]
headerEditor args opts =
  [ Grid.bigCell
      [ div_
          [ style_
              [ ("display", "flex"),
                ("align-items", "center"),
                ("justify-content", "center")
              ]
          ]
          [ Field.dynamicField
              args
              ( opts
                  & #optsFilledOrOutlined
                  .~ Outlined
              )
          ]
      ]
  ]

headerViewer :: Unicode -> [View action] -> [View action]
headerViewer txt widgets =
  if txt == mempty
    then mempty
    else
      [ LayoutGrid.cell
          [ LayoutGrid.span12,
            Typography.headline5,
            style_
              [ ("display", "flex"),
                ("text-align", "center"),
                ("align-items", "center"),
                ("justify-content", "center")
              ]
          ]
          $ [ text txt
            ]
          <> widgets
      ]

headerWrapper :: [View action] -> [View action]
headerWrapper xs =
  if null xs
    then mempty
    else [LayoutGrid.cell [LayoutGrid.span12] xs]
