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
  ALens' Model CurrencyInput ->
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
        . #currencyInputInfo,
      Dialog.dialog
        ( Dialog.config
            & Dialog.setOnClose closed
            & Dialog.setOpen
              ( st
                  ^. cloneLens optic
                  . #currencyInputOpen
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
              [ TextField.outlined
                  . TextField.setType (Just "text")
                  . ( if st
                        ^. cloneLens optic
                        . #currencyInputOpen
                        then id
                        else
                          TextField.setValue
                            ( Just
                                . from @Text @String
                                $ st
                                ^. cloneLens optic
                                . #currencyInputSearch
                            )
                    )
                  . TextField.setOnInput search
                  . TextField.setPlaceholder
                    ( Just
                        . inspectCurrencyInfo
                        $ st
                        ^. cloneLens optic
                        . #currencyInputInfo
                    )
                  . TextField.setAttributes
                    [ class_ "fill",
                      id_ . ms $ htmlUuid @Text uuid,
                      onKeyDown $ Misc.onKeyDownAction uuid
                    ]
                  $ TextField.config,
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
    uuid = st ^. cloneLens optic . #currencyInputUuid
    search input =
      pureUpdate 0 $ \st' ->
        st'
          & cloneLens optic
          . #currencyInputSearch
          .~ from @String @Text input
    opened =
      pureUpdate 0 $ \st' ->
        st'
          & cloneLens optic
          . #currencyInputOpen
          .~ True
          & cloneLens optic
          . #currencyInputSearch
          .~ mempty
    closed =
      pureUpdate 0 $ \st' ->
        st'
          & cloneLens optic
          . #currencyInputOpen
          .~ False
          & cloneLens optic
          . #currencyInputSearch
          .~ mempty

currencyListWidget ::
  Model ->
  ALens' Model CurrencyInput ->
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
    current = st ^. cloneLens optic . #currencyInputInfo
    search = st ^. cloneLens optic . #currencyInputSearch
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
  ALens' Model CurrencyInput ->
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
                . #currencyInputOpen
                .~ False
                & cloneLens optic
                . #currencyInputSearch
                .~ mempty
                & cloneLens optic
                . #currencyInputInfo
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
                ^. #modelData
                . #dataModelTopMoney
                . #moneyModelCurrency
                . #currencyInputInfo
            quoteCurrency =
              st
                ^. #modelData
                . #dataModelBottomMoney
                . #moneyModelCurrency
                . #currencyInputInfo
         in st
              & #modelData
              . #dataModelTopMoney
              . #moneyModelCurrency
              . #currencyInputInfo
              .~ quoteCurrency
              & #modelData
              . #dataModelBottomMoney
              . #moneyModelCurrency
              . #currencyInputInfo
              .~ baseCurrency
              & #modelData
              . #dataModelTopOrBottom
              .~ Top
