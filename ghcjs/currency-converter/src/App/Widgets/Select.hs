module App.Widgets.Select
  ( Opts (..),
    Item (..),
    defOpts,
    select,
  )
where

import Functora.Prelude as Prelude
import Miso hiding (view)
import Miso.String (fromMisoString, ms)

data Opts action = Opts
  { optsLabel :: Maybe Text,
    optsLeadingIcon :: Maybe Text,
    optsExtraAttributes :: [Attribute action],
    optsItems :: [Item],
    optsSelected :: Maybe Item,
    optsOnChange :: Maybe (Text -> action)
  }
  deriving stock (Generic)

data Item = Item
  { itemLabel :: Text,
    itemLeadingIcon :: Maybe Text,
    itemValue :: Text
  }
  deriving stock (Generic)

defOpts :: Opts action
defOpts =
  Opts
    { optsLabel = Nothing,
      optsLeadingIcon = Nothing,
      optsExtraAttributes = mempty,
      optsItems = mempty,
      optsSelected = Nothing,
      optsOnChange = Nothing
    }

select :: Opts action -> View action
select opts =
  div_
    [ class_ "select"
    ]
    [ select_
        ( maybeToList (onChange . (. fromMisoString) <$> opts ^. #optsOnChange)
            <> (opts ^. #optsExtraAttributes)
        )
        $ fmap
          ( \item ->
              option_
                [ value_ . ms $ item ^. #itemValue
                ]
                [ text . ms $ item ^. #itemLabel
                ]
          )
          ( opts ^. #optsItems
          )
    ]
