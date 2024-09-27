module Functora.Miso.Capa.Currency
  ( Args (..),
    Opts (..),
    defOpts,
    selectCurrency,
  )
where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Functora.Miso.Capa.Field as Field
import qualified Functora.Miso.Css as Css
import Functora.Miso.Prelude
import Functora.Miso.Types
import Functora.Money hiding (Currency, Money, Text)
import qualified Material.Button as Button
import qualified Material.Dialog as Dialog
import qualified Material.LayoutGrid as LayoutGrid
import qualified Material.List as List
import qualified Material.List.Item as ListItem
import qualified Miso
import qualified Text.Fuzzy as Fuzzy

data Args model action = Args
  { argsModel :: model,
    argsOptic :: ATraversal' model (Currency Unique),
    argsAction :: (model -> JSM model) -> action,
    argsEmitter :: (model -> JSM model) -> JSM (),
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

selectCurrency :: Args model action -> Opts model -> View action
selectCurrency
  args@Args
    { argsModel = st,
      argsOptic = optic,
      argsAction = action,
      argsEmitter = emitter
    }
  opts@Opts {optsExtraOnClick = extraOnClick} =
    LayoutGrid.cell
      [ LayoutGrid.span6Desktop
      ]
      $ [ Button.raised
            ( Button.setOnClick opened
                . Button.setAttributes
                  [ Css.fullWidth
                  ]
                $ Button.config
            )
            . ( optsButtonViewer opts
              )
            $ fromMaybe
              (CurrencyInfo (CurrencyCode "XXX") mempty)
              (st ^? cloneTraversal optic . #currencyOutput)
        ]
      <> if st ^? cloneTraversal optic . #currencyModalState /= Just Opened
        then mempty
        else
          [ Dialog.dialog
              ( Dialog.config
                  & Dialog.setOnClose closed
                  & Dialog.setOpen True
              )
              ( Dialog.dialogContent
                  Nothing
                  [ currencyListWidget args opts
                  ]
                  [ div_
                      [ Css.fullWidth
                      ]
                      [ Field.textField
                          Field.Args
                            { Field.argsModel = st,
                              Field.argsOptic =
                                cloneTraversal optic
                                  . #currencyInput,
                              Field.argsAction = action,
                              Field.argsEmitter = emitter
                            }
                          ( Field.defOpts
                              & #optsPlaceholder
                              .~ "Search"
                          ),
                        Button.raised
                          ( Button.config
                              & Button.setOnClick closed
                              & Button.setAttributes
                                [ Css.fullWidth
                                ]
                          )
                          "Back"
                      ]
                  ]
              )
          ]
    where
      opened =
        action $ \prev ->
          pure
            $ prev
            & cloneTraversal optic
            . #currencyModalState
            .~ Opened
            & cloneTraversal optic
            . #currencyInput
            . #fieldInput
            . #uniqueValue
            .~ mempty
            & extraOnClick
      closed =
        action $ \prev ->
          pure
            $ prev
            & cloneTraversal optic
            . #currencyModalState
            .~ Closed
            & cloneTraversal optic
            . #currencyInput
            . #fieldInput
            . #uniqueValue
            .~ mempty

currencyListWidget :: Args model action -> Opts model -> View action
currencyListWidget
  args@Args
    { argsModel = st,
      argsOptic = optic,
      argsCurrencies = currencies
    }
  opts =
    List.list
      List.config
      ( currencyListItemWidget args opts current
          $ maybe (newFuzz current) NonEmpty.head matching
      )
      . fmap (currencyListItemWidget args opts current)
      $ maybe mempty NonEmpty.tail matching
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
        if search == mempty
          then Just . fmap newFuzz $ st ^. currencies
          else
            nonEmpty
              --
              -- TODO : filter not by exact word order,
              -- but by all possible permutations as well,
              -- with bigger priority for original query.
              --
              $ Fuzzy.filter
                search
                ( toList $ st ^. currencies
                )
                "<b>"
                "</b>"
                inspectCurrencyInfo
                False

currencyListItemWidget ::
  Args model action ->
  Opts model ->
  CurrencyInfo ->
  Fuzzy.Fuzzy CurrencyInfo Unicode ->
  ListItem.ListItem action
currencyListItemWidget
  Args
    { argsOptic = optic,
      argsAction = action
    }
  Opts
    { optsExtraOnClick = extraOnClick
    }
  current
  fuzz =
    ListItem.listItem
      ( ListItem.config
          & ListItem.setSelected
            ( if current == item
                then Just ListItem.activated
                else Nothing
            )
          & ListItem.setOnClick
            ( action $ \st ->
                pure
                  $ st
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
            )
      )
      [ Miso.rawHtml
          $ Fuzzy.rendered fuzz
      ]
    where
      item = Fuzzy.original fuzz
