module App.MainWidget (mainWidget) where

import App.AmountWidget
import App.CurrencyWidget
import qualified App.Misc as Misc
import App.Types
import qualified Data.Text as T
import qualified Data.Version as Version
import Functora.Prelude as Prelude
import qualified Material.Button as Button
import qualified Material.LayoutGrid as LayoutGrid
import qualified Material.Snackbar as Snackbar
import qualified Material.Theme as Theme
import qualified Material.Typography as Typography
import Miso hiding (view)
import Miso.String hiding (cons, foldl, intercalate, null, reverse)
import qualified Paths_app as Paths

mainWidget :: Model -> View Action
mainWidget st =
  LayoutGrid.layoutGrid
    [ LayoutGrid.alignMiddle
    ]
    $ [ LayoutGrid.inner
          ( [ class_ "container"
            ]
              --
              -- NOTE : Hiding widget on the first render to avoid flickering.
              --
              <> ( if st ^. #modelHide
                    then [style_ [("display", "none")]]
                    else mempty
                 )
          )
          ( screenWidget st
              <> [
                   -- LayoutGrid.cell [LayoutGrid.span12]
                   --   . (: mempty)
                   --   $ div_ mempty [inspect $ st ^. #modelData],
                   swapScreenWidget st,
                   tosWidget,
                   Snackbar.snackbar (Snackbar.config Misc.snackbarClosed)
                    $ modelSnackbarQueue st
                 ]
          )
      ]
    <> ( if st ^. #modelHide
          then [div_ [class_ "lds-dual-ring"] mempty]
          else mempty
       )

screenWidget :: Model -> [View Action]
screenWidget st@Model {modelScreen = Converter} =
  [ amountWidget st Top,
    currencyWidget st $ Misc.getConverterMoneyLens Top,
    amountWidget st Bottom,
    currencyWidget st $ Misc.getConverterMoneyLens Bottom,
    swapAmountsWidget,
    swapCurrenciesWidget
  ]
screenWidget st@Model {modelScreen = InvoiceEditor} =
  [ amountWidget st Top,
    currencyWidget st $ Misc.getConverterMoneyLens Top,
    currencyWidget st
      $ #modelData
      . #modelDataPaymentMethodsInput
      . #paymentMethodMoney
  ]

swapScreenWidget :: Model -> View Action
swapScreenWidget st =
  LayoutGrid.cell
    [ LayoutGrid.span6Desktop,
      LayoutGrid.span8Tablet,
      LayoutGrid.span4Phone
    ]
    . (: mempty)
    . Button.raised
      ( Button.setOnClick onClickAction
          . Button.setAttributes
            [ class_ "fill",
              Theme.secondaryBg
            ]
          $ Button.config
      )
    $ case st ^. #modelScreen of
      Converter -> "Create invoice"
      InvoiceEditor -> "Show converter"
  where
    onClickAction =
      PushUpdate
        ( do
            --
            -- NOTE : Need to sync text inputs on new screen.
            --
            sleepMilliSeconds 300
            Misc.pushActionQueue st $ ChanItem 0 id
        )
        $ ChanItem
          0
          ( &
              #modelScreen
                %~ ( \case
                      Converter -> InvoiceEditor
                      InvoiceEditor -> Converter
                   )
          )

tosWidget :: View Action
tosWidget =
  LayoutGrid.cell
    [ LayoutGrid.span12,
      Typography.subtitle2,
      style_
        [ ("text-align", "center")
        ]
    ]
    [ Miso.text "\169 2024 Functora. All rights reserved. ",
      Miso.text "By continuing to use this software, you agree to the ",
      a_ [href_ "license.html"] [Miso.text "Terms of Service"],
      Miso.text " and ",
      a_ [href_ "privacy.html"] [Miso.text "Privacy Policy"],
      Miso.text ". ",
      Miso.text . ms $ "Version " <> vsn <> "."
    ]

vsn :: Text
vsn =
  T.intercalate "."
    . fmap inspect
    $ Version.versionBranch Paths.version