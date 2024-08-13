module App.Widgets.Main (mainWidget) where

import qualified App.Misc as Misc
import App.Types
import qualified App.Widgets.Decrypt as Decrypt
import qualified App.Widgets.Header as Header
import qualified App.Widgets.Menu as Menu
import qualified App.Widgets.SwapAmounts as SwapAmounts
import qualified App.Widgets.SwapCurrencies as SwapCurrencies
import qualified Functora.Miso.Jsm as Jsm
import Functora.Miso.Prelude
import qualified Functora.Miso.Widgets.BrowserLink as BrowserLink
import qualified Functora.Miso.Widgets.Currency as Currency
import qualified Functora.Miso.Widgets.Field as Field
import qualified Functora.Miso.Widgets.FieldPairs as FieldPairs
import qualified Functora.Miso.Widgets.Qr as Qr
import Functora.Money hiding (Text)
import qualified Material.Button as Button
import qualified Material.LayoutGrid as LayoutGrid
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
          [ class_ "container",
            TopAppBar.shortFixedAdjust
          ]
          ( Menu.menu st
              <> screenWidget st
              <> [ -- LayoutGrid.cell [LayoutGrid.span12]
                   --  . (: mempty)
                   --  $ div_
                   --    mempty
                   --    [ text . inspect $ st ^. #modelFavMap
                   --    ],
                   tosWidget
                 ]
          )
      ]
    <> ( if st ^. #modelLoading
          then
            [ div_
                [ class_
                    "mdc-dialog mdc-dialog--fullscreen fullscreen-dialog mdc-dialog--open mdc-dialog-scroll-divider-footer mdc-dialog--scrollable"
                ]
                [ div_ [class_ "mdc-dialog__scrim"] mempty,
                  div_ [class_ "lds-dual-ring"] mempty
                ]
            ]
          else mempty
       )

screenWidget :: Model -> [View Action]
screenWidget st@Model {modelState = St {stCpt = Just {}}} =
  case st ^. #modelState . #stScreen of
    QrCode sc ->
      Header.headerWrapper
        ( Field.dynamicFieldViewer pushUpdate (st ^. #modelState . #stPre)
        )
        <> Qr.qr
          Qr.Args
            { Qr.argsValue =
                toMisoString
                  . either impureThrow URI.render
                  . stUri
                  $ st
                  & #modelState
                  . #stScreen
                  %~ unQrCode,
              Qr.argsAction =
                PushUpdate
                  . fmap InstantChanItem
            }
          ( Qr.defOpts @Action
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
        ( Field.dynamicFieldViewer pushUpdate (st ^. #modelState . #stPre)
        )
        <> Qr.qr
          Qr.Args
            { Qr.argsValue = toMisoString $ URI.render uri,
              Qr.argsAction = PushUpdate . fmap InstantChanItem
            }
          ( Qr.defOpts @Action
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
  let amountWidget' loc =
        Field.ratioField
          Field.Args
            { Field.argsModel = st,
              Field.argsOptic = Misc.getConverterAmountOptic loc,
              Field.argsAction = pushUpdate
            }
          ( Field.defOpts @Model @Action
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
      currencyWidget' tob =
        Currency.selectCurrency
          Currency.Args
            { Currency.argsModel = st,
              Currency.argsOptic = Misc.getConverterCurrencyOptic tob,
              Currency.argsAction = pushUpdate,
              Currency.argsCurrencies = #modelCurrencies
            }
   in ( FieldPairs.fieldPairsViewer
          FieldPairs.Args
            { FieldPairs.argsModel = st,
              FieldPairs.argsOptic =
                #modelState
                  . #stDoc
                  . #stDocFieldPairs,
              FieldPairs.argsAction = pushUpdate
            }
      )
        <> [ amountWidget' Top,
             currencyWidget' Top,
             amountWidget' Bottom,
             currencyWidget' Bottom,
             SwapAmounts.swapAmounts,
             SwapCurrencies.swapCurrencies,
             ratesWidget st
           ]

ratesWidget :: Model -> View Action
ratesWidget st =
  LayoutGrid.cell
    [ LayoutGrid.span12,
      Typography.caption,
      style_
        $ [("text-align", "center")]
        <> ( case oof of
              Offline -> [("color", "#B00020")]
              Online -> mempty
           )
    ]
    [ a_
        [ style_ [("cursor", "pointer")],
          onClick . pushUpdate $ Jsm.shareText msg
        ]
        [ Miso.text msg
        ]
    ]
  where
    oof = st ^. #modelState . #stDoc . #stDocOnlineOrOffline
    top = st ^. #modelState . #stDoc . #stDocTopMoney
    topAmt = top ^. #moneyAmount . #fieldOutput
    topCur =
      top
        ^. #moneyCurrency
        . #currencyOutput
        . #currencyInfoCode
        . #unCurrencyCode
        . to toMisoString
    bottom = st ^. #modelState . #stDoc . #stDocBottomMoney
    bottomAmt = bottom ^. #moneyAmount . #fieldOutput
    bottomCur =
      bottom
        ^. #moneyCurrency
        . #currencyOutput
        . #currencyInfoCode
        . #unCurrencyCode
        . to toMisoString
    bottomPerTop =
      if topAmt == 0
        then 0
        else bottomAmt / topAmt
    topPerBottom =
      if bottomAmt == 0
        then 0
        else topAmt / bottomAmt
    msg =
      inspect oof
        <> " exchange rates"
        <> ( case oof of
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
        <> ": 1 "
        <> topCur
        <> " \8776 "
        <> inspectRatioDef bottomPerTop
        <> " "
        <> bottomCur
        <> ", 1 "
        <> bottomCur
        <> " \8776 "
        <> inspectRatioDef topPerBottom
        <> " "
        <> topCur
        <> "."

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
    [ Miso.text "\169 2024 ",
      BrowserLink.browserLink
        BrowserLink.Args
          { BrowserLink.argsLink = "https://functora.github.io/",
            BrowserLink.argsLabel = "Functora",
            BrowserLink.argsAction = pushUpdate
          },
      Miso.text ". All rights reserved. ",
      Miso.text "By continuing to use this software, you agree to the ",
      a_ [href_ "license.html"] [Miso.text "Terms of Service"],
      Miso.text " and ",
      a_ [href_ "privacy.html"] [Miso.text "Privacy Policy"],
      Miso.text $ ". Version " <> vsn <> "."
    ]
