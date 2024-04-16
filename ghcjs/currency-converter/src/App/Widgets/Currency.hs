module App.Widgets.Currency
  ( currencySelect,
    currencySwap,
  )
where

import App.Types
import qualified App.Widgets.TextInput as TextInput
import qualified Data.List.NonEmpty as NonEmpty
import Functora.Money
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
  ALens' Model (Currency Unique) ->
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
        $ st
        ^. cloneLens optic
        . #currencyOutput,
      Dialog.dialog
        ( Dialog.config
            & Dialog.setOnClose closed
            & Dialog.setOpen (st ^. cloneLens optic . #currencyOpen)
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
                  ( inspectCurrencyInfo
                      $ st
                      ^. cloneLens optic
                      . #currencyOutput
                  )
                  ( cloneLens optic
                      . #currencyInput
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
          & cloneLens optic
          . #currencyOpen
          .~ True
          & cloneLens optic
          . #currencyInput
          . #uniqueValue
          .~ mempty
    closed =
      pureUpdate 0 $ \st' ->
        st'
          & cloneLens optic
          . #currencyOpen
          .~ False
          & cloneLens optic
          . #currencyInput
          . #uniqueValue
          .~ mempty

currencyListWidget ::
  Model ->
  ALens' Model (Currency Unique) ->
  View Action
currencyListWidget st optic =
  List.list
    List.config
    ( currencyListItemWidget optic current
        $ maybe current NonEmpty.head matching
    )
    . fmap (currencyListItemWidget optic current)
    $ maybe mempty NonEmpty.tail matching
  where
    currencies = st ^. #modelCurrencies
    current = st ^. cloneLens optic . #currencyOutput
    search = st ^. cloneLens optic . #currencyInput . #uniqueValue
    matching =
      nonEmpty
        . fmap Fuzzy.original
        $ Fuzzy.filter
          search
          (toList currencies)
          "<"
          ">"
          inspectCurrencyInfo
          False

currencyListItemWidget ::
  ALens' Model (Currency Unique) ->
  CurrencyInfo ->
  CurrencyInfo ->
  ListItem.ListItem Action
currencyListItemWidget optic current item =
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
                & cloneLens optic
                . #currencyOpen
                .~ False
                & cloneLens optic
                . #currencyInput
                . #uniqueValue
                .~ mempty
                & cloneLens optic
                . #currencyOutput
                .~ item
          )
    )
    [ Miso.text . toMisoString $ inspectCurrencyInfo @Text item
    ]

currencySwap :: View Action
currencySwap =
  LayoutGrid.cell
    [ LayoutGrid.span6Desktop,
      LayoutGrid.span4Tablet,
      LayoutGrid.span2Phone
    ]
    . (: mempty)
    $ Button.raised
      ( Button.setOnClick onClickAction
          . Button.setAttributes
            [ class_ "fill",
              Theme.secondaryBg
            ]
          $ Button.config
      )
      "Swap currencies"
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
