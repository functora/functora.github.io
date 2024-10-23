module Functora.Miso.Widgets.Flex
  ( flexRow,
    flexCol,
    flexLeftRight,
  )
where

import Functora.Miso.Prelude

flexRow ::
  ([Attribute action] -> [View action] -> View action) ->
  ([Attribute action] -> [Attribute action]) ->
  [View action] ->
  View action
flexRow newTag newAttr =
  newTag
    $ newAttr
      [ style_
          [ ("display", "flex"),
            ("flex-wrap", "wrap"),
            ("flex-direction", "row"),
            ("align-items", "center"),
            ("justify-content", "center")
          ]
      ]

flexCol ::
  ([Attribute action] -> [View action] -> View action) ->
  ([Attribute action] -> [Attribute action]) ->
  [View action] ->
  View action
flexCol newTag newAttr =
  newTag
    $ newAttr
      [ style_
          [ ("display", "flex"),
            ("flex-direction", "column")
          ]
      ]

flexLeftRight ::
  ([Attribute action] -> [View action] -> View action) ->
  ([Attribute action] -> [Attribute action]) ->
  [View action] ->
  [View action] ->
  [View action]
flexLeftRight newTag newAttr lhs rhs
  | null lhs && null rhs = mempty
  | otherwise =
      singleton
        . newTag
          ( newAttr
              [ style_
                  [ ("display", "flex"),
                    ("flex-wrap", "wrap"),
                    ("flex-direction", "row"),
                    ("align-items", "center"),
                    ("justify-content", "space-between")
                  ]
              ]
          )
        $ lhs
        <> [ div_
              [ style_
                  [ ("flex-grow", "1")
                  ]
              ]
              mempty
           ]
        <> rhs
