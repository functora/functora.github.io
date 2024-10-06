module Functora.Miso.Widgets.Money
  ( Args (..),
    Opts (..),
    defOpts,
    moneyViewer,
  )
where

import Functora.Miso.Prelude
import Functora.Miso.Types
import qualified Functora.Miso.Widgets.Field as Field
import Functora.Money hiding (Currency, Money, Text)

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
      ( if label == mempty
          then mempty
          else [strong_ mempty [text label]]
      )
        <> Field.ratioField
          Field.Args
            { Field.argsModel = st,
              Field.argsOptic = cloneTraversal optic . #moneyAmount,
              Field.argsAction = action,
              Field.argsEmitter = error "TODO_MONEY_EMITTER"
            }
          ( Field.defOpts
              & #optsDisabled
              .~ True
              & #optsPlaceholder
              .~ inspectCurrencyInfo
                ( money ^. #moneyCurrency . #currencyOutput
                )
          )
  where
    label =
      opts ^. #optsLabel
