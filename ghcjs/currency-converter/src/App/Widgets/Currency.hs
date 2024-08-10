module App.Widgets.Currency
  ( Args (..),
    selectCurrency,
  )
where

import qualified Data.List.NonEmpty as NonEmpty
import Functora.Miso.Prelude
import Functora.Miso.Types
import qualified Functora.Miso.Widgets.Field as Field
import Functora.Money hiding (Currency, Money, Text)
import qualified Functora.Prelude as Prelude
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
    argsAction :: JSM (model -> model) -> action,
    argsCurrencies :: Getter' model (NonEmpty CurrencyInfo)
  }
  deriving stock (Generic)

selectCurrency :: Args model action -> View action
selectCurrency
  args@Args
    { argsModel = st,
      argsOptic = optic,
      argsAction = action
    } =
    LayoutGrid.cell
      [ LayoutGrid.span6Desktop
      ]
      $ [ Button.raised
            ( Button.setOnClick opened
                . Button.setAttributes
                  [ class_ "fill"
                  ]
                $ Button.config
            )
            . inspectCurrencyInfo
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
                  [ currencyListWidget args
                  ]
                  [ div_
                      [ class_ "fill"
                      ]
                      [ Field.textField
                          Field.Args
                            { Field.argsModel = st,
                              Field.argsOptic =
                                cloneTraversal optic
                                  . #currencyInput,
                              Field.argsAction = action
                            }
                          ( Field.defOpts
                              & #optsPlaceholder
                              .~ "Search"
                          ),
                        Button.raised
                          ( Button.config
                              & Button.setOnClick closed
                              & Button.setAttributes
                                [ class_ "fill"
                                ]
                          )
                          "Back"
                      ]
                  ]
              )
          ]
    where
      opened =
        action . pure $ \prev ->
          prev
            --
            -- TODO : Widget first render is slow for some reason.
            -- Blocking view makes it look a bit better.
            -- Need to make widget first render fast.
            --
            -- & #modelLoading
            -- .~ True
            & cloneTraversal optic
            . #currencyModalState
            .~ Opened
            & cloneTraversal optic
            . #currencyInput
            . #fieldInput
            . #uniqueValue
            .~ mempty
      closed =
        action . pure $ \prev ->
          prev
            & cloneTraversal optic
            . #currencyModalState
            .~ Closed
            & cloneTraversal optic
            . #currencyInput
            . #fieldInput
            . #uniqueValue
            .~ mempty

currencyListWidget :: Args model action -> View action
currencyListWidget
  args@Args
    { argsModel = st,
      argsOptic = optic,
      argsCurrencies = currencies
    } =
    List.list
      List.config
      ( currencyListItemWidget args current
          $ maybe (newFuzz current) NonEmpty.head matching
      )
      . fmap (currencyListItemWidget args current)
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
        maybe
          mempty
          fromMisoString
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
  CurrencyInfo ->
  Fuzzy.Fuzzy CurrencyInfo Prelude.Text ->
  ListItem.ListItem action
currencyListItemWidget
  Args
    { argsOptic = optic,
      argsAction = action
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
            ( action . pure $ \st ->
                st
                  -- & #modelLoading
                  -- .~ True
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
            )
      )
      [ Miso.rawHtml
          . toMisoString
          $ Fuzzy.rendered fuzz
      ]
    where
      item = Fuzzy.original fuzz
