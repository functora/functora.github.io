module App.Widgets.Currency
  ( currencySelect,
    currencySwap,
  )
where

import App.Types
import qualified App.Widgets.TextInput as TextInput
import qualified Data.List.NonEmpty as NonEmpty
import Functora.Money hiding (Currency)
import Functora.Prelude as Prelude
import qualified Material.Button as Button
import qualified Material.Dialog as Dialog
import qualified Material.LayoutGrid as LayoutGrid
import qualified Material.List as List
import qualified Material.List.Item as ListItem
import qualified Material.Theme as Theme
import Miso hiding (view)
import Miso.String hiding (cons, foldl, intercalate, null, reverse)
import qualified Text.Fuzzy as Fuzzy

currencySelect ::
  Model ->
  ATraversal' Model (Currency Unique) ->
  View Action
currencySelect st optic =
  LayoutGrid.cell
    [ LayoutGrid.span6Desktop
    ]
    [ Button.raised
        ( Button.setOnClick opened
            . Button.setAttributes
              [ class_ "fill"
              ]
            $ Button.config
        )
        . inspectCurrencyInfo
        $ fromMaybe
          (unexpectedCurrency ^. #currencyOutput)
          (st ^? cloneTraversal optic . #currencyOutput),
      Dialog.dialog
        ( Dialog.config
            & Dialog.setOnClose closed
            & Dialog.setOpen
              ( fromMaybe
                  (unexpectedCurrency ^. #currencyOpen)
                  (st ^? cloneTraversal optic . #currencyOpen)
              )
        )
        ( Dialog.dialogContent
            Nothing
            [ currencyListWidget st optic
            ]
            . (: mempty)
            $ div_
              [ class_ "fill"
              ]
              [ TextInput.textInput
                  st
                  ( cloneTraversal optic
                      . #currencyInput
                  )
                  ( TextInput.opts
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
                  "Cancel"
              ]
        )
    ]
  where
    opened =
      pureUpdate 0 $ \st' ->
        st'
          & cloneTraversal optic
          . #currencyOpen
          .~ True
          & cloneTraversal optic
          . #currencyInput
          . #uniqueValue
          .~ mempty
    closed =
      pureUpdate 0 $ \st' ->
        st'
          & cloneTraversal optic
          . #currencyOpen
          .~ False
          & cloneTraversal optic
          . #currencyInput
          . #uniqueValue
          .~ mempty

currencyListWidget ::
  Model ->
  ATraversal' Model (Currency Unique) ->
  View Action
currencyListWidget st optic =
  List.list
    List.config
    ( currencyListItemWidget optic current
        $ maybe currentFuzz NonEmpty.head matching
    )
    . fmap (currencyListItemWidget optic current)
    $ maybe mempty NonEmpty.tail matching
  where
    current =
      fromMaybe
        (unexpectedCurrency ^. #currencyOutput)
        (st ^? cloneTraversal optic . #currencyOutput)
    currentFuzz =
      Fuzzy.Fuzzy
        { Fuzzy.original = current,
          Fuzzy.rendered = inspectCurrencyInfo current,
          Fuzzy.score = 0
        }
    search =
      fromMaybe
        (unexpectedCurrency ^. #currencyInput . #uniqueValue)
        (st ^? cloneTraversal optic . #currencyInput . #uniqueValue)
    matching =
      nonEmpty
        --
        -- TODO : filter not by exact word order,
        -- but by all possible permutations as well,
        -- with bigger priority for original query.
        --
        $ Fuzzy.filter
          search
          ( toList $ st ^. #modelCurrencies
          )
          "<b>"
          "</b>"
          inspectCurrencyInfo
          False

currencyListItemWidget ::
  ATraversal' Model (Currency Unique) ->
  CurrencyInfo ->
  Fuzzy.Fuzzy CurrencyInfo Text ->
  ListItem.ListItem Action
currencyListItemWidget optic current fuzz =
  ListItem.listItem
    ( ListItem.config
        & ListItem.setSelected
          ( if current == item
              then Just ListItem.activated
              else Nothing
          )
        & ListItem.setOnClick
          ( pureUpdate 0 $ \st ->
              st
                & cloneTraversal optic
                . #currencyOpen
                .~ False
                & cloneTraversal optic
                . #currencyInput
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

currencySwap :: View Action
currencySwap =
  LayoutGrid.cell
    [ LayoutGrid.span6Desktop,
      LayoutGrid.span4Tablet,
      LayoutGrid.span2Phone
    ]
    [ Button.raised
        ( Button.config
            & Button.setOnClick onClickAction
            & Button.setAttributes
              [ class_ "fill",
                Theme.secondaryBg
              ]
        )
        "Swap currencies"
    ]
  where
    onClickAction =
      pureUpdate 0 $ \st ->
        let baseCurrency =
              st
                ^. #modelState
                . #stateTopMoney
                . #moneyCurrency
                . #currencyOutput
            quoteCurrency =
              st
                ^. #modelState
                . #stateBottomMoney
                . #moneyCurrency
                . #currencyOutput
         in st
              & #modelState
              . #stateTopMoney
              . #moneyCurrency
              . #currencyOutput
              .~ quoteCurrency
              & #modelState
              . #stateBottomMoney
              . #moneyCurrency
              . #currencyOutput
              .~ baseCurrency
              & #modelState
              . #stateTopOrBottom
              .~ Top

unexpectedCurrency :: Currency Unique
unexpectedCurrency =
  Currency
    { currencyOpen = False,
      currencyInput = Unique nilUid "UNEXPECTED CURRENCY",
      currencyOutput =
        CurrencyInfo
          { currencyInfoCode = CurrencyCode "UNEXPECTED",
            currencyInfoText = "CURRENCY"
          }
    }
