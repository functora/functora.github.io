module App.Widgets.Button
  ( bigButton,
    mediumButton,
    smallButton,
  )
where

import Functora.Prelude
import qualified Material.Button as Button
import qualified Material.LayoutGrid as LayoutGrid
import Miso hiding (at, view)

bigButton ::
  forall a action.
  ( From a String
  ) =>
  [Attribute action] ->
  a ->
  action ->
  View action
bigButton =
  button @a @action
    [ LayoutGrid.span12
    ]

mediumButton ::
  forall a action.
  ( From a String
  ) =>
  [Attribute action] ->
  a ->
  action ->
  View action
mediumButton =
  button @a @action
    [ LayoutGrid.span6Desktop,
      LayoutGrid.span4Tablet,
      LayoutGrid.span2Phone
    ]

smallButton ::
  forall a action.
  ( From a String
  ) =>
  [Attribute action] ->
  a ->
  action ->
  View action
smallButton =
  button @a @action
    [ LayoutGrid.span3Desktop,
      LayoutGrid.span2Tablet,
      LayoutGrid.span2Phone
    ]

button ::
  forall a action.
  ( From a String
  ) =>
  [Attribute action] ->
  [Attribute action] ->
  a ->
  action ->
  View action
button external internal label action =
  LayoutGrid.cell
    external
    [ Button.raised
        ( Button.setOnClick action
            . Button.setAttributes (class_ "fill" : internal)
            $ Button.config
        )
        ( from @a @String label
        )
    ]
