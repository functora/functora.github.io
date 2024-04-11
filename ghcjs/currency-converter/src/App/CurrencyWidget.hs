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
  ALens' Model CurrencyModel ->
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
        . #currencyModelData,
      Dialog.dialog
        ( Dialog.config
            & Dialog.setOnClose closed
            & Dialog.setOpen
              ( st
                  ^. cloneLens optic
                  . #currencyModelOpen
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
                        . #currencyModelOpen
                        then id
                        else
                          TextField.setValue
                            ( Just
                                . from @Text @String
                                $ st
                                ^. cloneLens optic
                                . #currencyModelSearch
                            )
                    )
                  . TextField.setOnInput search
                  . TextField.setPlaceholder
                    ( Just
                        . inspectCurrencyInfo
                        $ st
                        ^. cloneLens optic
                        . #currencyModelData
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
    uuid = st ^. cloneLens optic . #currencyModelUuid
    search input =
      pureUpdate 0 $ \st' ->
        st'
          & cloneLens optic
          . #currencyModelSearch
          .~ from @String @Text input
    opened =
      pureUpdate 0 $ \st' ->
        st'
          & cloneLens optic
          . #currencyModelOpen
          .~ True
          & cloneLens optic
          . #currencyModelSearch
          .~ mempty
    closed =
      pureUpdate 0 $ \st' ->
        st'
          & cloneLens optic
          . #currencyModelOpen
          .~ False
          & cloneLens optic
          . #currencyModelSearch
          .~ mempty

currencyListWidget ::
  Model ->
  ALens' Model CurrencyModel ->
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
    current = st ^. cloneLens optic . #currencyModelData
    search = st ^. cloneLens optic . #currencyModelSearch
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
  ALens' Model CurrencyModel ->
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
                . #currencyModelOpen
                .~ False
                & cloneLens optic
                . #currencyModelSearch
                .~ mempty
                & cloneLens optic
                . #currencyModelData
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
                . #currencyModelData
            quoteCurrency =
              st
                ^. #modelData
                . #dataModelBottomMoney
                . #moneyModelCurrency
                . #currencyModelData
         in st
              & #modelData
              . #dataModelTopMoney
              . #moneyModelCurrency
              . #currencyModelData
              .~ quoteCurrency
              & #modelData
              . #dataModelBottomMoney
              . #moneyModelCurrency
              . #currencyModelData
              .~ baseCurrency
              & #modelData
              . #dataModelTopOrBottom
              .~ Top
