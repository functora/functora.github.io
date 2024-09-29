module Functora.Miso.Widgets.Currency
  ( Args (..),
    Opts (..),
    defOpts,
    selectCurrency,
  )
where

import qualified Functora.Miso.Widgets.Dialog as Dialog
import qualified Functora.Miso.Widgets.Field as Field
import Functora.Miso.Prelude
import Functora.Miso.Types
import Functora.Money hiding (Currency, Money, Text)
import qualified Miso
import qualified Text.Fuzzy as Fuzzy

data Args model action = Args
  { argsModel :: model,
    argsOptic :: ATraversal' model (Currency Unique),
    argsAction :: Update model -> action,
    argsEmitter :: Update model -> JSM (),
    argsCurrencies :: Getter' model (NonEmpty CurrencyInfo)
  }
  deriving stock (Generic)

data Opts model = Opts
  { optsExtraOnClick :: model -> model,
    optsButtonViewer :: CurrencyInfo -> Unicode
  }
  deriving stock (Generic)

defOpts :: Opts model
defOpts =
  Opts
    { optsExtraOnClick = id,
      optsButtonViewer = inspectCurrencyInfo
    }

selectCurrency :: Opts model -> Args model action -> View action
selectCurrency
  opts@Opts
    { optsExtraOnClick = extraOnClick
    }
  args@Args
    { argsModel = st,
      argsOptic = optic,
      argsAction = action,
      argsEmitter = emitter
    } =
    div_ mempty
      $ [ button_ [onClick open]
            . singleton
            . text
            . (opts ^. #optsButtonViewer)
            $ fromMaybe
              (CurrencyInfo (CurrencyCode "XXX") mempty)
              (st ^? cloneTraversal optic . #currencyOutput)
        ]
      <> Dialog.dialog
        Dialog.defOpts
          { Dialog.optsExtraOnClose =
              cloneTraversal optic
                . #currencyInput
                . #fieldInput
                . #uniqueValue
                .~ mempty
          }
        Dialog.Args
          { Dialog.argsModel = st,
            Dialog.argsOptic = cloneTraversal optic . #currencyModalState,
            Dialog.argsAction = action,
            Dialog.argsContent =
              [ currencyListWidget opts args,
                Field.textField
                  Field.Args
                    { Field.argsModel = st,
                      Field.argsOptic = cloneTraversal optic . #currencyInput,
                      Field.argsAction = action,
                      Field.argsEmitter = emitter
                    }
                  ( Field.defOpts
                      & #optsPlaceholder
                      .~ "Search"
                  )
              ]
          }
    where
      open =
        action . PureUpdate $ \prev ->
          prev
            & cloneTraversal optic
            . #currencyModalState
            .~ Opened
            & cloneTraversal optic
            . #currencyInput
            . #fieldInput
            . #uniqueValue
            .~ mempty
            & extraOnClick

currencyListWidget :: Opts model -> Args model action -> View action
currencyListWidget
  opts
  args@Args
    { argsModel = st,
      argsOptic = optic,
      argsCurrencies = currencies
    } =
    ul_ [class_ "tree-view"]
      $ fmap (currencyListItemWidget opts args current) matching
    where
      current =
        fromMaybe
          (CurrencyInfo (CurrencyCode "XXX") mempty)
          (st ^? cloneTraversal optic . #currencyOutput)
      newFuzz cur =
        Fuzzy.Fuzzy
          { Fuzzy.original = cur,
            Fuzzy.rendered = inspectCurrencyInfo cur,
            Fuzzy.score = 0
          }
      search =
        fromMaybe
          mempty
          $ st
          ^? cloneTraversal optic
          . #currencyInput
          . #fieldInput
          . #uniqueValue
      matching =
        --
        -- TODO : filter not by exact word order,
        -- but by all possible permutations as well,
        -- with bigger priority for original query.
        --
        if search == mempty
          then fmap newFuzz . toList $ st ^. currencies
          else
            Fuzzy.filter
              search
              (toList $ st ^. currencies)
              "<b>"
              "</b>"
              inspectCurrencyInfo
              False

currencyListItemWidget ::
  Opts model ->
  Args model action ->
  CurrencyInfo ->
  Fuzzy.Fuzzy CurrencyInfo Unicode ->
  View action
currencyListItemWidget
  Opts
    { optsExtraOnClick = extraOnClick
    }
  Args
    { argsOptic = optic,
      argsAction = action
    }
  current
  fuzz =
    li_
      [ onClick . action . PureUpdate $ \st ->
          st
            & cloneTraversal optic
            . #currencyModalState
            .~ Closed
            & cloneTraversal optic
            . #currencyInput
            . #fieldInput
            . #uniqueValue
            .~ mempty
            & cloneTraversal optic
            . #currencyOutput
            .~ item
            & extraOnClick
      ]
      [ ( if current == item
            then strong_ mempty
            else span_ mempty
        )
          . singleton
          . Miso.rawHtml
          $ Fuzzy.rendered fuzz
      ]
    where
      item = Fuzzy.original fuzz
