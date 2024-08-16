module Functora.Miso.Widgets.Header
  ( headerEditor,
    headerViewer,
    headerWrapper,
  )
where

import Functora.Miso.Prelude
import Functora.Miso.Types
import qualified Functora.Miso.Widgets.Field as Field
import qualified Functora.Miso.Widgets.Grid as Grid
import qualified Material.LayoutGrid as LayoutGrid
import qualified Material.Typography as Typography

headerEditor ::
  Field.Args model action DynamicField ->
  Field.Opts model action ->
  [View action]
headerEditor args opts =
  [ Grid.bigCell
      $ div_
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

headerViewer :: MisoString -> [View action]
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

headerWrapper :: [View action] -> [View action]
headerWrapper xs =
  if null xs
    then mempty
    else [LayoutGrid.cell [LayoutGrid.span12] xs]
