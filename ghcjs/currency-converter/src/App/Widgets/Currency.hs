module App.Widgets.Currency
  ( Opts (..),
    defOpts,
    moneyViewer,
    selectCurrency,
    swapCurrencies,
  )
where

import App.Types
import qualified App.Widgets.Button as Button
import qualified App.Widgets.Cell as Cell
import qualified App.Widgets.Field as Field
import qualified App.Widgets.Modal as Modal
import qualified Data.List.NonEmpty as NonEmpty
import Functora.Money hiding (Currency, Money)
import Functora.Prelude as Prelude
import qualified Material.LayoutGrid as LayoutGrid
import qualified Material.List as List
import qualified Material.List.Item as ListItem
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
      --
      -- TODO : remove this, seems redundant?
      --
      optsShowZeroAmount = True
    }

moneyViewer :: Model -> Opts -> Money Unique -> [View Action]
moneyViewer st opts money =
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
          [ class_ "fill",
            style_ [("text-align", "center")]
          ]
          [ Field.constTextField
              st
              ( inspectRatioDef $ money ^. #moneyAmount . #fieldOutput
              )
              ( Field.defOpts
                  & #optsPlaceholder
                  .~ inspectCurrencyInfo
                    ( money ^. #moneyCurrency . #currencyOutput
                    )
              )
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
    [ Button.button
        ( Button.defOpts
            & #optsOnClick
            .~ opened
            & #optsLabel
            .~ Just
              ( inspectCurrencyInfo
                  $ fromMaybe
                    (unexpectedCurrency ^. #currencyOutput)
                    (st ^? cloneTraversal optic . #currencyOutput)
              )
        ),
      Modal.modal
        st
        ( Modal.defOpts
            & #optsExtraOnClose
            .~ clearInput
        )
        ( cloneTraversal optic
            . #currencyModalState
        )
        [ currencyListWidget st optic,
          Field.textField
            st
            ( cloneTraversal optic
                . #currencyInput
            )
            ( Field.defOpts
                & #optsPlaceholder
                .~ "Search"
            ),
          Button.button
            ( Button.defOpts
                & #optsOnClick
                .~ closed
                & #optsLabel
                .~ Just "Back"
            )
        ]
    ]
  where
    opened =
      pureUpdate 0
        $ clearInput
        . ( &
              cloneTraversal optic
                . #currencyModalState
                .~ Opened
          )
    closed =
      pureUpdate 0
        $ clearInput
        . ( &
              cloneTraversal optic
                . #currencyModalState
                .~ Closed
          )
    clearInput =
      ( &
          cloneTraversal optic
            . #currencyInput
            . #fieldInput
            . #uniqueValue
            .~ mempty
      )

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

swapCurrencies :: View Action
swapCurrencies =
  LayoutGrid.cell
    [ LayoutGrid.span6Desktop,
      LayoutGrid.span4Tablet,
      LayoutGrid.span2Phone
    ]
    [ Button.button
        ( Button.defOpts
            & #optsOnClick
            .~ onClickAction
            & #optsLeadingIcon
            .~ Just "arrows-up-down"
            & #optsLabel
            .~ Just "Swap currencies"
        )
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
    { currencyInput =
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
          },
      currencyModalState = Closed
    }
