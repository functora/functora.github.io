module App.Widgets.Switch
  ( Opts (..),
    opts,
    switch,
  )
where

import App.Types
import Functora.Prelude as Prelude
import qualified Material.DataTable as DataTable
import qualified Material.Icon as Icon
import qualified Material.LayoutGrid as LayoutGrid
import qualified Material.Switch as Switch
import qualified Material.Typography as Typography
import Miso hiding (at, view)
import Miso.String hiding (cons, foldl, intercalate, null, reverse)

data Opts = Opts
  { optsDisabled :: Bool,
    optsPlaceholder :: Text,
    optsIcon :: Maybe Text
  }
  deriving stock (Generic)

opts :: Opts
opts =
  Opts
    { optsDisabled = False,
      optsPlaceholder = mempty,
      optsIcon = Nothing
    }

switch :: Model -> Opts -> ATraversal' Model Bool -> View Action
switch st options optic =
  DataTable.dataTable
    ( DataTable.config
        & DataTable.setAttributes
          [ class_ "fill",
            class_ "mdc-button--touch"
          ]
    )
    [ DataTable.row
        mempty
        [ DataTable.cell
            [ Typography.body1,
              LayoutGrid.alignMiddle,
              style_
                [ ("display", "flex"),
                  ("align-items", "center"),
                  ("justify-content", "space-between")
                ]
            ]
            $ maybeToList
              ( fmap (Icon.icon mempty . from @Text @String)
                  $ optsIcon options
              )
            <> [ Miso.rawHtml "&nbsp;",
                 Miso.text . ms $ options ^. #optsPlaceholder,
                 Miso.rawHtml "&nbsp;&nbsp;",
                 Switch.switch
                  $ Switch.config
                  & Switch.setChecked
                    ( fromMaybe False $ st ^? cloneTraversal optic
                    )
                  & Switch.setOnChange
                    ( pureUpdate 0 (& cloneTraversal optic %~ not)
                    )
               ]
        ]
    ]
    mempty
