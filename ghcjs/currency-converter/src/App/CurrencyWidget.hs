module App.CurrencyWidget
  ( currencyWidget,
    swapCurrenciesWidget,
  )
where

import qualified App.Misc as Misc
import App.Types
import qualified Data.List.NonEmpty as NonEmpty
import Functora.Money
import Functora.Prelude as Prelude
import qualified Material.Button as Button
import qualified Material.Dialog as Dialog
import qualified Material.LayoutGrid as LayoutGrid
import qualified Material.List as List
import qualified Material.List.Item as ListItem
import qualified Material.TextField as TextField
import qualified Material.Theme as Theme
import Miso hiding (view)
import Miso.String hiding (cons, foldl, intercalate, null, reverse)
import qualified Text.Fuzzy as Fuzzy

currencyWidget ::
  Model ->
  ALens' Model (Currency Unique) ->
  View Action
currencyWidget st optic =
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
              [ TextField.outlined
                  $ TextField.config
                  & TextField.setType (Just "text")
                  & TextField.setOnInput search
                  & TextField.setPlaceholder
                    ( Just
                        . inspectCurrencyInfo
                        $ st
                        ^. cloneLens optic
                        . #currencyOutput
                    )
                  & TextField.setAttributes
                    [ class_ "fill",
                      id_ . ms $ htmlUuid @Text uuid,
                      onKeyDown $ Misc.onKeyDownAction uuid
                    ],
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
    uuid = st ^. cloneLens optic . #currencyInput . #uniqueUuid
    search input =
      pureUpdate 0 $ \st' ->
        st'
          & cloneLens optic
          . #currencyInput
          . #uniqueValue
          .~ from @String @Text input
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

swapCurrenciesWidget :: View Action
swapCurrenciesWidget =
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
