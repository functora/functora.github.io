module App.Widgets.Header
  ( header,
    subHeader,
    navHeaderComplex,
    navHeaderSimple,
  )
where

import qualified App.Misc as Misc
import App.Types
import Functora.Prelude
import qualified Material.Fab as Fab
import qualified Material.IconButton as IconButton
import qualified Material.LayoutGrid as LayoutGrid
import qualified Material.Typography as Typography
import Miso hiding (at, view)
import Miso.String hiding (cons, foldl, intercalate, null, reverse)

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
              )
              "add"
          ]
      )
      action

subHeader :: Text -> View Action
subHeader txt =
  LayoutGrid.cell
    [ LayoutGrid.span6Desktop,
      LayoutGrid.span4Tablet,
      LayoutGrid.span4Phone,
      Typography.headline5,
      style_
        [ ("display", "flex"),
          ("align-items", "center"),
          ("justify-content", "center"),
          ("text-align", "center")
        ]
    ]
    [ Miso.text $ ms txt
    ]

data Nav = Nav
  { navDown :: Action,
    navUp :: Action,
    navDuplicate :: Action,
    navRemove :: Action,
    navAdd :: Maybe Action
  }
  deriving stock (Generic)

navHeaderComplex ::
  ( Data a
  ) =>
  Model ->
  ATraversal' Model [a] ->
  ATraversal' a [TextProp Unique] ->
  Int ->
  View Action
navHeaderComplex st optic props idx =
  --
  -- TODO : implement all
  --
  navHeader
    Nav
      { navDown = Noop,
        navUp = Noop,
        navDuplicate = Misc.duplicateAt st optic idx,
        navRemove = Misc.removeAt st optic idx,
        navAdd =
          Just
            . Misc.newTextPropAction
            $ cloneTraversal optic
            . ix idx
            . props
      }

navHeaderSimple ::
  ( Data a
  ) =>
  Model ->
  ATraversal' Model [a] ->
  Int ->
  View Action
navHeaderSimple st optic idx =
  --
  -- TODO : implement all
  --
  navHeader
    Nav
      { navDown = Noop,
        navUp = Noop,
        navDuplicate = Misc.duplicateAt st optic idx,
        navRemove = Misc.removeAt st optic idx,
        navAdd = Nothing
      }

navHeader :: Nav -> View Action
navHeader nav =
  LayoutGrid.cell
    [ LayoutGrid.span6Desktop,
      LayoutGrid.span4Tablet,
      LayoutGrid.span4Phone,
      Typography.headline5,
      style_
        [ ("display", "flex"),
          ("align-items", "center"),
          ("justify-content", "space-between"),
          ("text-align", "center")
        ]
    ]
    $ [ IconButton.iconButton
          ( IconButton.config
              & IconButton.setOnClick (navDown nav)
          )
          "keyboard_double_arrow_down",
        IconButton.iconButton
          ( IconButton.config
              & IconButton.setOnClick (navUp nav)
          )
          "keyboard_double_arrow_up",
        IconButton.iconButton
          ( IconButton.config
              & IconButton.setOnClick (navDuplicate nav)
          )
          "library_add",
        IconButton.iconButton
          ( IconButton.config
              & IconButton.setOnClick (navRemove nav)
          )
          "delete_forever"
      ]
    <> maybe
      mempty
      ( \action ->
          [ IconButton.iconButton
              ( IconButton.config
                  & IconButton.setOnClick action
              )
              "post_add"
          ]
      )
      (navAdd nav)
