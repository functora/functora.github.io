module Functora.Miso.Widgets.Currency
  ( Args (..),
    Opts (..),
    defOpts,
    selectCurrency,
  )
where

import Functora.Miso.Prelude
import Functora.Miso.Types
import qualified Functora.Miso.Widgets.Dialog as Dialog
import qualified Functora.Miso.Widgets.Field as Field
import qualified Functora.Miso.Widgets.Icon as Icon
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
  { optsButtonLabel :: Maybe Unicode,
    optsButtonViewer :: CurrencyInfo -> Unicode,
    optsExtraOnClick :: model -> model
  }
  deriving stock (Generic)

defOpts :: Opts model
defOpts =
  Opts
    { optsButtonLabel = Nothing,
      optsButtonViewer = inspectCurrencyInfo,
      optsExtraOnClick = id
    }

selectCurrency ::
  forall model action.
  Opts model ->
  Args model action ->
  [View action]
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
    [ maybe
        id
        ( \x ->
            label_ mempty
              . (text x :)
              . (br_ mempty :)
              . singleton
        )
        (optsButtonLabel opts)
        $ input_
          [ type_ "button",
            onClick open,
            value_
              . optsButtonViewer opts
              $ fromMaybe
                (CurrencyInfo (CurrencyCode "XXX") mempty)
                (st ^? cloneTraversal optic . #currencyOutput)
          ]
    ]
      <> Dialog.dialog
        Dialog.defOpts
          { Dialog.optsTitle = Just "Currency",
            Dialog.optsTitleIcon = Just Icon.IconCoins,
            Dialog.optsFlexContent = False,
            Dialog.optsExtraOnClose =
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
              Field.textField
                Field.Args
                  { Field.argsModel = st,
                    Field.argsOptic = cloneTraversal optic . #currencyInput,
                    Field.argsAction = action,
                    Field.argsEmitter = emitter
                  }
                ( Field.defOpts @model @action
                    & #optsPlaceholder
                    .~ ("Search" :: Unicode)
                    & #optsExtraAttributes
                    .~ [autofocus_ True]
                )
                <> [br_ mempty]
                <> currencyListWidget opts args
          }
    where
      open =
        action
          . PureUpdate
          $ \prev ->
            prev
              & cloneTraversal optic
              . #currencyModalState
              .~ Opened
              & cloneTraversal optic
              . #currencyInput
              . #fieldInput
              . #uniqueValue
              .~ mempty
              & cloneTraversal optic
              . #currencyInput
              . #fieldOutput
              .~ mempty
              & extraOnClick

currencyListWidget :: Opts model -> Args model action -> [View action]
currencyListWidget
  opts
  args@Args
    { argsModel = st,
      argsOptic = optic,
      argsCurrencies = currencies
    } =
    fmap (currencyListItemWidget opts args current) matching
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
    button_
      ( [ onClick
            . action
            . PureUpdate
            $ \st ->
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
          <> ( if current == item
                then [type_ "submit"]
                else mempty
             )
      )
      [ Miso.rawHtml
          $ Fuzzy.rendered fuzz
      ]
    where
      item = Fuzzy.original fuzz
