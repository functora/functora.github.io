module App.Widgets.Currency
  ( Opts (..),
    defOpts,
    moneyViewer,
    selectCurrency,
    swapCurrencies,
  )
where

import App.Types
import qualified App.Widgets.Cell as Cell
import qualified App.Widgets.Field as Field
import qualified Data.List.NonEmpty as NonEmpty
import Functora.Money hiding (Currency, Money)
import Functora.Prelude as Prelude
import qualified Material.Button as Button
import qualified Material.Dialog as Dialog
import qualified Material.LayoutGrid as LayoutGrid
import qualified Material.List as List
import qualified Material.List.Item as ListItem
import qualified Material.Theme as Theme
import qualified Material.Typography as Typography
import Miso hiding (view)
import Miso.String hiding (cons, foldl, intercalate, null, reverse)
import qualified Text.Fuzzy as Fuzzy

data Opts = Opts
  { optsLabel :: Text,
    optsShowZeroAmount :: Bool
  }
  deriving stock (Generic)

defOpts :: Opts
defOpts =
  Opts
    { optsLabel = "Amount",
      optsShowZeroAmount = True
    }

moneyViewer :: Opts -> Money Unique -> [View Action]
moneyViewer opts money =
  catMaybes
    [ if null label
        then Nothing
        else
          Just
            . cell
            $ strong_ [Typography.typography] [text $ ms label],
      Just
        . cell
        $ div_
          [Typography.typography]
          [ text
              . ms
              $ (money ^. #moneyAmount . #fieldInput . #uniqueValue)
              <> " "
              <> inspectCurrencyInfo (money ^. #moneyCurrency . #currencyOutput)
          ]
    ]
  where
    label = opts ^. #optsLabel
    cell =
      if null label
        then Cell.bigCell
        else Cell.mediumCell

selectCurrency ::
  Model ->
  ATraversal' Model (Currency Unique) ->
  View Action
selectCurrency st optic =
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
              [ Field.textField
                  st
                  ( cloneTraversal optic
                      . #currencyInput
                  )
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
          . #fieldInput
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
          . #fieldInput
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
        (unexpectedCurrency ^. #currencyInput . #fieldInput . #uniqueValue)
        (st ^? cloneTraversal optic . #currencyInput . #fieldInput . #uniqueValue)
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

swapCurrencies :: View Action
swapCurrencies =
  LayoutGrid.cell
    [ LayoutGrid.span6Desktop,
      LayoutGrid.span4Tablet,
      LayoutGrid.span2Phone
    ]
    [ Button.raised
        ( Button.config
            & Button.setIcon (Just "swap_vertical_circle")
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
                . #stConv
                . #stConvTopMoney
                . #moneyCurrency
                . #currencyOutput
            quoteCurrency =
              st
                ^. #modelState
                . #stConv
                . #stConvBottomMoney
                . #moneyCurrency
                . #currencyOutput
         in st
              & #modelState
              . #stConv
              . #stConvTopMoney
              . #moneyCurrency
              . #currencyOutput
              .~ quoteCurrency
              & #modelState
              . #stConv
              . #stConvBottomMoney
              . #moneyCurrency
              . #currencyOutput
              .~ baseCurrency
              & #modelState
              . #stConv
              . #stConvTopOrBottom
              .~ Top

unexpectedCurrency :: Currency Unique
unexpectedCurrency =
  Currency
    { currencyOpen = False,
      currencyInput =
        Field
          { fieldType = FieldTypeText,
            fieldInput = Unique nilUid "UNEXPECTED CURRENCY",
            fieldOutput = "UNEXPECTED CURRENCY",
            fieldAllowCopy = True,
            fieldModalState = Closed
          },
      currencyOutput =
        CurrencyInfo
          { currencyInfoCode = CurrencyCode "UNEXPECTED",
            currencyInfoText = "CURRENCY"
          }
    }
