module App.Widgets.Main (mainWidget) where

import qualified App.Misc as Misc
import App.Types
import qualified App.Widgets.Assets as Assets
import qualified App.Widgets.Cell as Cell
import qualified App.Widgets.Currency as Currency
import qualified App.Widgets.Field as Field
import qualified App.Widgets.FieldPairs as FieldPairs
import qualified App.Widgets.PaymentMethods as PaymentMethods
import qualified App.Widgets.SwapAmounts as SwapAmounts
import qualified Data.Text as T
import qualified Data.Version as Version
import Functora.Money
import Functora.Prelude as Prelude
import qualified Material.Button as Button
import qualified Material.LayoutGrid as LayoutGrid
import qualified Material.Snackbar as Snackbar
import qualified Material.Theme as Theme
import qualified Material.Typography as Typography
import Miso hiding (view)
import Miso.String (ms)
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
              <> [ -- LayoutGrid.cell [LayoutGrid.span12]
                   --  . (: mempty)
                   --  $ div_
                   --    mempty
                   --    [ inspect . newIdentityState $ st ^. #modelState
                   --    ],
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
  let amountWidget' loc =
        Field.ratioField @Rational
          st
          ( Left
              $ Misc.getConverterAmountOptic loc
          )
          ( Field.defOpts
              & #optsExtraOnInput
              .~ (& #modelState . #stateTopOrBottom .~ loc)
              & #optsPlaceholder
              .~ ( st
                    ^. cloneLens (Misc.getConverterCurrencyOptic loc)
                    . #currencyOutput
                    . to (inspectCurrencyInfo @Text)
                 )
          )
      currencyWidget' =
        Currency.selectCurrency st
          . cloneLens
          . Misc.getConverterCurrencyOptic
   in [ amountWidget' Top,
        currencyWidget' Top,
        amountWidget' Bottom,
        currencyWidget' Bottom,
        SwapAmounts.swapAmounts,
        Currency.swapCurrencies
      ]
screenWidget st@Model {modelScreen = DocumentEditor} =
  FieldPairs.fieldPairs Header st (#modelState . #stateFieldPairs)
    <> Assets.assets st (#modelState . #stateAssets)
    <> PaymentMethods.paymentMethods st (#modelState . #statePaymentMethods)

swapScreenWidget :: Model -> View Action
swapScreenWidget st =
  Cell.bigCell
    $ Button.raised
      ( Button.config
          & Button.setAttributes
            [ Theme.secondaryBg,
              class_ "fill"
            ]
          & Button.setOnClick
            ( pureUpdate
                0
                ( &
                    #modelScreen
                      %~ ( \case
                            Converter -> DocumentEditor
                            DocumentEditor -> Converter
                         )
                )
            )
      )
      ( case st ^. #modelScreen of
          Converter -> "Create invoice"
          DocumentEditor -> "Show converter"
      )

tosWidget :: View Action
tosWidget =
  LayoutGrid.cell
    [ LayoutGrid.span12,
      Typography.caption,
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
