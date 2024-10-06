module Functora.Miso.Widgets.Flex
  ( flex,
    flexLeftRight,
  )
where

import Functora.Miso.Prelude

flex ::
  ([Attribute action] -> [View action] -> View action) ->
  ([Attribute action] -> [Attribute action]) ->
  [View action] ->
  View action
flex newTag newAttr =
  newTag
    $ newAttr
      [ style_ [("display", "flex")]
      ]

flexLeftRight ::
  ([Attribute action] -> [View action] -> View action) ->
  ([Attribute action] -> [Attribute action]) ->
  [View action] ->
  [View action] ->
  [View action]
flexLeftRight newTag newAttr lhs rhs
  | null lhs && null rhs = mempty
  | null lhs || null rhs = singleton . flex newTag newAttr $ lhs <> rhs
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
