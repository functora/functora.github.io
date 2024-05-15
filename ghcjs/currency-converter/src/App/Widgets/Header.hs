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
import qualified Material.Theme as Theme
import qualified Material.Typography as Typography
import Miso hiding (at, view)
import Miso.String (ms)

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
    navAdd :: Maybe Action,
    navButtonStyle :: [Attribute Action]
  }
  deriving stock (Generic)

navHeaderComplex ::
  ( Data a
  ) =>
  Model ->
  ATraversal' Model [a] ->
  ATraversal' a [FieldPair DynamicField Unique] ->
  Int ->
  [Attribute Action] ->
  View Action
navHeaderComplex st optic fields idx attrs =
  --
  -- TODO : implement all
  --
  navHeader
    ( if null attrs
        then
          [ LayoutGrid.span6Desktop,
            LayoutGrid.span4Tablet,
            LayoutGrid.span4Phone
          ]
        else attrs
    )
    Nav
      { navDown = Noop,
        navUp = Noop,
        navDuplicate = Misc.duplicateAt st optic idx,
        navRemove = Misc.removeAt st optic idx,
        navAdd =
          Just
            . Misc.newFieldPairAction
            $ cloneTraversal optic
            . ix idx
            . fields,
        navButtonStyle =
          [ Theme.primary
          ]
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
    [ LayoutGrid.span6Desktop,
      LayoutGrid.span4Tablet,
      LayoutGrid.span4Phone
    ]
    Nav
      { navDown = Noop,
        navUp = Noop,
        navDuplicate = Misc.duplicateAt st optic idx,
        navRemove = Misc.removeAt st optic idx,
        navAdd = Nothing,
        navButtonStyle = mempty
      }

navHeader :: [Attribute Action] -> Nav -> View Action
navHeader style nav =
  LayoutGrid.cell
    ( style
        <> [ style_
              [ ("display", "flex"),
                ("align-items", "center"),
                ("justify-content", "space-between"),
                ("text-align", "center")
              ]
           ]
    )
    $ [ IconButton.iconButton
          ( IconButton.config
              & IconButton.setOnClick (navDown nav)
              & IconButton.setAttributes buttonAttrs
          )
          "keyboard_double_arrow_down",
        IconButton.iconButton
          ( IconButton.config
              & IconButton.setOnClick (navUp nav)
              & IconButton.setAttributes buttonAttrs
          )
          "keyboard_double_arrow_up",
        IconButton.iconButton
          ( IconButton.config
              & IconButton.setOnClick (navDuplicate nav)
              & IconButton.setAttributes buttonAttrs
          )
          "library_add",
        IconButton.iconButton
          ( IconButton.config
              & IconButton.setOnClick (navRemove nav)
              & IconButton.setAttributes buttonAttrs
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
  where
    buttonAttrs = navButtonStyle nav
