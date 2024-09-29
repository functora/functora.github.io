module Functora.Miso.Widgets.Money
  ( Args (..),
    Opts (..),
    defOpts,
    moneyViewer,
  )
where

import qualified Functora.Miso.Widgets.Field as Field
import qualified Functora.Miso.Widgets.Grid as Grid
import qualified Functora.Miso.Css as Css
import Functora.Miso.Prelude
import Functora.Miso.Types
import Functora.Money hiding (Currency, Money, Text)
import qualified Material.Typography as Typography

data Args model action = Args
  { argsModel :: model,
    argsOptic :: ATraversal' model (Money Unique),
    argsAction :: Update model -> action
  }
  deriving stock (Generic)

data Opts = Opts
  { optsLabel :: Unicode,
    optsShowZeroAmount :: Bool
  }
  deriving stock (Generic)

defOpts :: Opts
defOpts =
  Opts
    { optsLabel = "Amount",
      --
      -- TODO : remove this, seems redundant?
      --
      optsShowZeroAmount = True
    }

moneyViewer :: Args model action -> Opts -> [View action]
moneyViewer Args {argsModel = st, argsOptic = optic, argsAction = action} opts =
  case st ^? cloneTraversal optic of
    Nothing -> mempty
    Just money ->
      catMaybes
        [ if label == mempty
            then Nothing
            else
              Just
                $ cell
                  [ strong_ [Typography.typography] [text label]
                  ],
          Just
            $ cell
              [ div_
                  [ Css.fullWidth,
                    style_ [("text-align", "center")]
                  ]
                  [ Field.constTextField
                      ( inspectRatioDef
                          $ money
                          ^. #moneyAmount
                          . #fieldOutput
                      )
                      ( Field.defOpts
                          & #optsPlaceholder
                          .~ inspectCurrencyInfo
                            ( money
                                ^. #moneyCurrency
                                . #currencyOutput
                            )
                      )
                      action
                  ]
              ]
        ]
  where
    cell =
      if label == mempty
        then Grid.bigCell
        else Grid.mediumCell
    label =
      opts ^. #optsLabel
