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
  ALens' Model ModelMoney ->
  View Action
currencyWidget st moneyLens =
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
        ^. cloneLens moneyLens
        . #modelMoneyCurrencyInfo,
      Dialog.dialog
        ( Dialog.config
            & Dialog.setOnClose closed
            & Dialog.setOpen
              ( st
                  ^. cloneLens moneyLens
                  . #modelMoneyCurrencyOpen
              )
        )
        ( Dialog.dialogContent
            Nothing
            [ currencyListWidget st moneyLens
            ]
            . (: mempty)
            $ div_
              [ class_ "fill"
              ]
              [ TextField.outlined
                  . TextField.setType (Just "text")
                  . ( if st
                        ^. cloneLens moneyLens
                        . #modelMoneyCurrencyOpen
                        then id
                        else
                          TextField.setValue
                            ( Just
                                . from @Text @String
                                $ st
                                ^. cloneLens moneyLens
                                . #modelMoneyCurrencySearch
                            )
                    )
                  . TextField.setOnInput search
                  . TextField.setPlaceholder
                    ( Just
                        . inspectCurrencyInfo
                        $ st
                        ^. cloneLens moneyLens
                        . #modelMoneyCurrencyInfo
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
    uuid = st ^. cloneLens moneyLens . #modelMoneyCurrencyUuid
    search input =
      pureUpdate 0 $ \st' ->
        st'
          & cloneLens moneyLens
          . #modelMoneyCurrencySearch
          .~ from @String @Text input
    opened =
      pureUpdate 0 $ \st' ->
        st'
          & cloneLens moneyLens
          . #modelMoneyCurrencyOpen
          .~ True
          & cloneLens moneyLens
          . #modelMoneyCurrencySearch
          .~ mempty
    closed =
      pureUpdate 0 $ \st' ->
        st'
          & cloneLens moneyLens
          . #modelMoneyCurrencyOpen
          .~ False
          & cloneLens moneyLens
          . #modelMoneyCurrencySearch
          .~ mempty

currencyListWidget ::
  Model ->
  ALens' Model ModelMoney ->
  View Action
currencyListWidget st moneyLens =
  List.list
    List.config
    ( currencyListItemWidget moneyLens current
        $ maybe current NonEmpty.head matching
    )
    . fmap (currencyListItemWidget moneyLens current)
    $ maybe mempty NonEmpty.tail matching
  where
    currencies = st ^. #modelCurrencies
    current = st ^. cloneLens moneyLens . #modelMoneyCurrencyInfo
    search = st ^. cloneLens moneyLens . #modelMoneyCurrencySearch
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
  ALens' Model ModelMoney ->
  CurrencyInfo ->
  CurrencyInfo ->
  ListItem.ListItem Action
currencyListItemWidget moneyLens current item =
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
                & cloneLens moneyLens
                . #modelMoneyCurrencyOpen
                .~ False
                & cloneLens moneyLens
                . #modelMoneyCurrencySearch
                .~ mempty
                & cloneLens moneyLens
                . #modelMoneyCurrencyInfo
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
              st ^. #modelData . #modelDataTopMoney . #modelMoneyCurrencyInfo
            quoteCurrency =
              st ^. #modelData . #modelDataBottomMoney . #modelMoneyCurrencyInfo
         in st
              & #modelData
              . #modelDataTopMoney
              . #modelMoneyCurrencyInfo
              .~ quoteCurrency
              & #modelData
              . #modelDataBottomMoney
              . #modelMoneyCurrencyInfo
              .~ baseCurrency
              & #modelData
              . #modelDataTopOrBottom
              .~ Top
