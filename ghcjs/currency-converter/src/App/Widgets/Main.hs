module App.Widgets.Main (mainWidget) where

import qualified App.Misc as Misc
import App.Types
import qualified App.Widgets.Currency as Currency
import qualified App.Widgets.Decrypt as Decrypt
import qualified App.Widgets.Field as Field
import qualified App.Widgets.FieldPairs as FieldPairs
import qualified App.Widgets.Header as Header
import qualified App.Widgets.Menu as Menu
import qualified App.Widgets.Qr as Qr
import qualified App.Widgets.SwapAmounts as SwapAmounts
import Functora.Miso.Prelude
import Functora.Money hiding (Text)
import qualified Material.Button as Button
import qualified Material.LayoutGrid as LayoutGrid
import qualified Material.Snackbar as Snackbar
import qualified Material.Theme as Theme
import qualified Material.TopAppBar as TopAppBar
import qualified Material.Typography as Typography
import Miso hiding (view)
import qualified Text.URI as URI

mainWidget :: Model -> View Action
mainWidget st =
  LayoutGrid.layoutGrid
    [ LayoutGrid.alignMiddle
    ]
    $ [ LayoutGrid.inner
          ( [ class_ "container",
              TopAppBar.shortFixedAdjust
            ]
              --
              -- NOTE : Hiding widget on the first render to avoid flickering.
              --
              <> ( if st ^. #modelLoading
                    then [style_ [("display", "none")]]
                    else mempty
                 )
          )
          ( Menu.menu st
              <> screenWidget st
              <> [ -- LayoutGrid.cell [LayoutGrid.span12]
                   --  . (: mempty)
                   --  $ div_
                   --    mempty
                   --    [ text . inspect $ st ^. #modelFavMap
                   --    ],
                   tosWidget,
                   Snackbar.snackbar (Snackbar.config Misc.textPopupClosed)
                    $ modelSnackbarQueue st
                 ]
          )
      ]
    <> ( if st ^. #modelLoading
          then [div_ [class_ "lds-dual-ring"] mempty]
          else mempty
       )

screenWidget :: Model -> [View Action]
screenWidget st@Model {modelState = St {stCpt = Just {}}} =
  case st ^. #modelState . #stScreen of
    QrCode sc ->
      Header.headerWrapper
        ( Field.dynamicFieldViewer st (st ^. #modelState . #stPre)
        )
        <> Qr.qr
          st
          ( toMisoString
              . either impureThrow URI.render
              . stUri
              $ st
              & #modelState
              . #stScreen
              %~ unQrCode
          )
          ( Qr.defOpts
              & #optsExtraWidgets
              .~ [ Button.raised
                    ( Button.config
                        & Button.setIcon (Just "login")
                        & Button.setAttributes [class_ "fill"]
                        & Button.setOnClick (setScreenAction $ unQrCode sc)
                    )
                    "Open"
                 ]
          )
    _ ->
      Decrypt.decrypt st
screenWidget st@Model {modelState = St {stScreen = QrCode sc}} =
  case stUri $ st & #modelState . #stScreen %~ unQrCode of
    Left e -> impureThrow e
    Right uri ->
      Header.headerWrapper
        ( Field.dynamicFieldViewer st (st ^. #modelState . #stPre)
        )
        <> Qr.qr
          st
          ( toMisoString $ URI.render uri
          )
          ( Qr.defOpts
              & #optsExtraWidgets
              .~ [ Button.raised
                    ( Button.config
                        & Button.setIcon (Just "login")
                        & Button.setAttributes [class_ "fill"]
                        & Button.setOnClick (setScreenAction $ unQrCode sc)
                    )
                    "Open"
                 ]
          )
screenWidget st@Model {modelState = St {stScreen = Converter}} =
  let conn =
        st
          ^. #modelState
          . #stDoc
          . #stDocOnlineOrOffline
      amountWidget' loc =
        Field.ratioField
          st
          ( Misc.getConverterAmountOptic loc
          )
          ( Field.defOpts
              & #optsExtraOnInput
              .~ ( &
                    #modelState
                      . #stDoc
                      . #stDocTopOrBottom
                      .~ loc
                 )
              & #optsPlaceholder
              .~ ( st
                    ^. cloneLens (Misc.getConverterCurrencyOptic loc)
                    . #currencyOutput
                    . to (inspectCurrencyInfo @MisoString)
                 )
          )
      currencyWidget' =
        Currency.selectCurrency st
          . cloneLens
          . Misc.getConverterCurrencyOptic
   in ( FieldPairs.fieldPairsViewer
          st
          ( st
              ^. #modelState
              . #stDoc
              . #stDocFieldPairs
          )
      )
        <> [ amountWidget' Top,
             currencyWidget' Top,
             amountWidget' Bottom,
             currencyWidget' Bottom,
             SwapAmounts.swapAmounts,
             Currency.swapCurrencies,
             LayoutGrid.cell
              [ LayoutGrid.span12,
                Typography.caption,
                style_
                  [ ("text-align", "center")
                  ]
              ]
              [ Miso.text
                  $ inspect conn
                  <> " exchange rate"
                  <> ( case conn of
                        Offline -> mempty
                        Online ->
                          " on "
                            <> ( st
                                  ^. #modelState
                                  . #stDoc
                                  . #stDocCreatedAt
                                  . to utctDay
                                  . to inspect
                               )
                     )
              ]
           ]

tosWidget :: View Action
tosWidget =
  LayoutGrid.cell
    [ LayoutGrid.span12,
      Typography.caption,
      Theme.textDisabledOnBackground,
      class_ "no-print",
      style_
        [ ("text-align", "center")
        ]
    ]
    [ Miso.text "\169 2024 Functora. All rights reserved. ",
      Miso.text "By continuing to use this software, you agree to the ",
      a_ [href_ "license.html"] [Miso.text "Terms of Service"],
      Miso.text " and ",
      a_ [href_ "privacy.html"] [Miso.text "Privacy Policy"],
      Miso.text
        ". This software is 100% organic and AI-free. It is built and tested exclusively by humans. ",
      Miso.text $ "Version " <> vsn <> "."
    ]
