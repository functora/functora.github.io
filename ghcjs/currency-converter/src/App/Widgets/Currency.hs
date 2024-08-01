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
import Functora.Miso.Prelude
import Functora.Money hiding (Currency, Money, Text)
import qualified Functora.Prelude as Prelude
import qualified Material.Button as Button
import qualified Material.Dialog as Dialog
import qualified Material.LayoutGrid as LayoutGrid
import qualified Material.List as List
import qualified Material.List.Item as ListItem
import qualified Material.Theme as Theme
import qualified Material.Typography as Typography
import qualified Miso
import qualified Text.Fuzzy as Fuzzy

data Opts = Opts
  { optsLabel :: MisoString,
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
    [ if label == mempty
        then Nothing
        else
          Just
            . cell
            $ strong_ [Typography.typography] [text label],
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
      if label == mempty
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
                [ currencyListWidget st optic
                ]
                [ div_
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
                ]
            )
        ]
  where
    opened =
      pureUpdate 0 $ \st' ->
        st'
          --
          -- TODO : Widget first render is slow for some reason.
          -- Blocking view makes it look a bit better.
          -- Need to make widget first render fast.
          --
          & #modelLoading
          .~ True
          & cloneTraversal optic
          . #currencyModalState
          .~ Opened
          & cloneTraversal optic
          . #currencyInput
          . #fieldInput
          . #uniqueValue
          .~ mempty
    closed =
      pureUpdate 0 $ \st' ->
        st'
          & cloneTraversal optic
          . #currencyModalState
          .~ Closed
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
        $ maybe (newFuzz current) NonEmpty.head matching
    )
    . fmap (currencyListItemWidget optic current)
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
        then Just . fmap newFuzz $ st ^. #modelCurrencies
        else
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
  Fuzzy.Fuzzy CurrencyInfo Prelude.Text ->
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
                & #modelLoading
                .~ True
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
    [ Button.raised
        ( Button.config
            & Button.setIcon (Just "swap_vertical_circle")
            & Button.setOnClick onClickAction
            & Button.setAttributes
              [ class_ "fill",
                class_ "no-print",
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
                . #stDoc
                . #stDocTopMoney
                . #moneyCurrency
                . #currencyOutput
            quoteCurrency =
              st
                ^. #modelState
                . #stDoc
                . #stDocBottomMoney
                . #moneyCurrency
                . #currencyOutput
         in st
              & #modelState
              . #stDoc
              . #stDocTopMoney
              . #moneyCurrency
              . #currencyOutput
              .~ quoteCurrency
              & #modelState
              . #stDoc
              . #stDocBottomMoney
              . #moneyCurrency
              . #currencyOutput
              .~ baseCurrency
              & #modelState
              . #stDoc
              . #stDocTopOrBottom
              .~ Top
