module App.Widgets.Header
  ( headerEditor,
    headerViewer,
    headerWrapper,
  )
where

import App.Types
import qualified App.Widgets.Cell as Cell
import qualified App.Widgets.Field as Field
import Functora.Prelude hiding (Field)
import Miso hiding (at, view)
import Miso.String (ms)

headerEditor ::
  Model ->
  ATraversal' Model (Field DynamicField Unique) ->
  Field.Opts ->
  [View Action]
headerEditor st optic opts =
  [ Cell.bigCell
      [ div_
          [ style_
              [ ("display", "flex"),
                ("align-items", "center"),
                ("justify-content", "center")
              ]
          ]
          [ Field.field
              st
              optic
              opts
              parseDynamicField
              inspectDynamicField
          ]
      ]
  ]

headerViewer :: Text -> [View Action]
headerViewer txt =
  if null txt
    then mempty
    else [Cell.bigCell [text $ ms txt]]

headerWrapper :: [View Action] -> [View Action]
headerWrapper xs =
  if null xs
    then mempty
    else [Cell.bigCell xs]
