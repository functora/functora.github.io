module App.Widgets.Menu
  ( menu,
  )
where

import App.Types
import qualified App.Widgets.Fav as Fav
import qualified App.Xlsx as Xlsx
import qualified Functora.Miso.Css as Css
import qualified Functora.Miso.Jsm as Jsm
import Functora.Miso.Prelude
import qualified Functora.Miso.Widgets.BrowserLink as BrowserLink
import qualified Functora.Miso.Widgets.Currency as Currency
import qualified Functora.Miso.Widgets.Dialog as Dialog
import qualified Functora.Miso.Widgets.Field as Field
import qualified Functora.Miso.Widgets.Icon as Icon
import qualified Functora.Miso.Widgets.Select as Select
import qualified Functora.Money as Money
import qualified Text.URI as URI

menu :: Model -> [View Action]
menu st =
  [ menu_
      [ class_ "no-print",
        style_
          [ ("display", "flex"),
            ("flex-wrap", "wrap"),
            ("flex-direction", "row"),
            ("justify-content", "space-between")
          ]
      ]
      [ li_
          [ role_ "button",
            onClick
              . PushUpdate
              . Instant
              . PureUpdate
              $ #modelMenu
              .~ Opened
          ]
          [ icon Icon.IconMenu
          ],
        li_
          [ role_ "button",
            onClick . PushUpdate . Instant . ImpureUpdate $ do
              doc <- liftIO newSt
              pure $ #modelState .~ doc
          ]
          [ text "Delivery Calculator"
          ],
        div_
          [ style_ [("flex-grow", "1")]
          ]
          mempty,
        li_
          [ role_ "button",
            onClick
              . PushUpdate
              . Instant
              . PureUpdate
              $ #modelFav
              .~ Opened
          ]
          [ icon Icon.IconFav
          ],
        li_
          [ role_ "button",
            onClick
              . PushUpdate
              . Instant
              . ImpureUpdate
              $ do
                Jsm.printCurrentPage "delivery-calculator"
                pure id
          ]
          [ icon Icon.IconPrint
          ],
        li_
          [ role_ "button",
            onClick
              . PushUpdate
              . Instant
              . ImpureUpdate
              $ do
                Jsm.saveFile
                  "delivery-calculator.xlsx"
                  "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
                  Xlsx.newXlsx
                pure id
          ]
          [ icon Icon.IconDownload
          ],
        li_
          [ role_ "button",
            onClick
              . PushUpdate
              . Instant
              . Jsm.shareText
              . from @String @Unicode
              . either impureThrow URI.renderStr
              $ stUri st
          ]
          [ icon Icon.IconShare
          ]
      ]
  ]
    <> Fav.fav st
    <> Dialog.dialog
      ( Dialog.defOpts
          & #optsTitle
          .~ Just "Settings"
      )
      Dialog.Args
        { Dialog.argsModel = st,
          Dialog.argsOptic = #modelMenu,
          Dialog.argsAction = PushUpdate . Instant,
          Dialog.argsContent =
            [ Currency.selectCurrency
                Currency.defOpts
                  { Currency.optsExtraOnClick =
                      (& #modelLoading .~ True),
                    Currency.optsButtonViewer =
                      mappend "Marketplace - "
                        . Money.inspectCurrencyCode
                        . Money.currencyInfoCode
                  }
                Currency.Args
                  { Currency.argsModel = st,
                    Currency.argsOptic =
                      #modelState . #stAssetCurrency,
                    Currency.argsAction =
                      PushUpdate . Instant,
                    Currency.argsEmitter =
                      pushActionQueue st . Instant,
                    Currency.argsCurrencies =
                      #modelCurrencies
                  },
              Currency.selectCurrency
                Currency.defOpts
                  { Currency.optsExtraOnClick =
                      (& #modelLoading .~ True),
                    Currency.optsButtonViewer =
                      mappend "Merchant - "
                        . Money.inspectCurrencyCode
                        . Money.currencyInfoCode
                  }
                Currency.Args
                  { Currency.argsModel = st,
                    Currency.argsOptic =
                      #modelState . #stMerchantCurrency,
                    Currency.argsAction =
                      PushUpdate . Instant,
                    Currency.argsEmitter =
                      pushActionQueue st . Instant,
                    Currency.argsCurrencies =
                      #modelCurrencies
                  },
              Select.select
                ( Select.defOpts
                    & #optsLabel
                    .~ Just "Exchange rate"
                )
                Select.Args
                  { Select.argsModel =
                      st,
                    Select.argsOptic =
                      #modelState . #stOnlineOrOffline,
                    Select.argsAction =
                      PushUpdate . Instant,
                    Select.argsOptions =
                      constTraversal $ enumerate @OnlineOrOffline
                  },
              Field.ratioField
                Field.Args
                  { Field.argsModel = st,
                    Field.argsOptic =
                      #modelState . #stExchangeRate,
                    Field.argsAction =
                      PushUpdate . Instant,
                    Field.argsEmitter =
                      pushActionQueue st . Instant
                  }
                ( let disabled =
                        st
                          ^. #modelState
                          . #stOnlineOrOffline
                          == Online
                   in Field.defOpts @Model @Action
                        & #optsDisabled
                        .~ disabled
                        & #optsPlaceholder
                        .~ ( "1 "
                              <> toUpper
                                ( Money.inspectCurrencyCode
                                    $ st
                                    ^. #modelState
                                    . #stAssetCurrency
                                    . #currencyOutput
                                    . #currencyInfoCode
                                )
                              <> " \8776 X "
                              <> toUpper
                                ( Money.inspectCurrencyCode
                                    $ st
                                    ^. #modelState
                                    . #stMerchantCurrency
                                    . #currencyOutput
                                    . #currencyInfoCode
                                )
                           )
                        & ( if disabled
                              then #optsTrailingWidget .~ Nothing
                              else id
                          )
                ),
              Field.dynamicField
                Field.Args
                  { Field.argsModel = st,
                    Field.argsOptic =
                      #modelState . #stMerchantFeePercent,
                    Field.argsAction =
                      PushUpdate . Instant,
                    Field.argsEmitter =
                      pushActionQueue st . Instant
                  }
                ( Field.defOpts
                    & #optsPlaceholder
                    .~ ("Merchant fee %" :: Unicode)
                ),
              Field.textField
                Field.Args
                  { Field.argsModel = st,
                    Field.argsOptic = #modelState . #stMerchantTele,
                    Field.argsAction = PushUpdate . Instant,
                    Field.argsEmitter = pushActionQueue st . Instant
                  }
                ( Field.defOpts
                    & #optsPlaceholder
                    .~ ("Merchant telegram" :: Unicode)
                ),
              Field.textField
                Field.Args
                  { Field.argsModel = st,
                    Field.argsOptic = #modelState . #stPreview,
                    Field.argsAction = PushUpdate . Instant,
                    Field.argsEmitter = pushActionQueue st . Instant
                  }
                ( Field.defOpts @Model @Action
                    & #optsPlaceholder
                    .~ ("QR title" :: Unicode)
                ),
              button_
                [ Css.fullWidth,
                  onClick
                    . screen
                    $ if isQrCode sc
                      then Main
                      else QrCode sc
                ]
                [ text
                    $ if isQrCode sc
                      then "Delivery Calculator"
                      else "QR"
                ]
            ]
              <> linksWidget st
        }
  where
    screen next =
      PushUpdate
        . Instant
        . PureUpdate
        $ (#modelMenu .~ Closed)
        . (#modelLoading .~ isQrCode next)
        . (#modelState . #stScreen .~ next)
    sc =
      st ^. #modelState . #stScreen

linksWidget :: Model -> [View Action]
linksWidget st =
  [ button_
      [ onClick openWidget,
        Css.fullWidth
      ]
      [ text "App"
      ]
  ]
    <> Dialog.dialog
      Dialog.defOpts
      Dialog.Args
        { Dialog.argsModel = st,
          Dialog.argsOptic = #modelLinks,
          Dialog.argsAction = PushUpdate . Instant,
          Dialog.argsContent =
            [ text
                "The Android app is in closed beta. To install it, join the ",
              BrowserLink.browserLink
                BrowserLink.Args
                  { BrowserLink.argsLink = testGroupLink,
                    BrowserLink.argsLabel = "closed beta",
                    BrowserLink.argsAction =
                      PushUpdate
                        . Instant
                  },
              text " group and then install the app from ",
              BrowserLink.browserLink
                BrowserLink.Args
                  { BrowserLink.argsLink = googlePlayLink,
                    BrowserLink.argsLabel = "Google Play",
                    BrowserLink.argsAction =
                      PushUpdate
                        . Instant
                  },
              text ", or download the ",
              BrowserLink.browserLink
                BrowserLink.Args
                  { BrowserLink.argsLink = apkLink,
                    BrowserLink.argsLabel = "APK file",
                    BrowserLink.argsAction =
                      PushUpdate
                        . Instant
                  },
              text " directly.",
              button_
                [ onClick $ openBrowser testGroupLink,
                  Css.fullWidth
                ]
                [ text "Join testing (closed beta)"
                ],
              button_
                [ onClick $ openBrowser googlePlayLink,
                  Css.fullWidth
                ]
                [ text "Google Play (closed beta)"
                ],
              button_
                [ onClick $ openBrowser apkLink,
                  Css.fullWidth
                ]
                [ text "Download APK"
                ],
              button_
                [ onClick $ openBrowser sourceLink,
                  Css.fullWidth
                ]
                [ text "Source"
                ],
              button_
                [ onClick $ openBrowser functoraLink,
                  Css.fullWidth
                ]
                [ text "Author"
                ],
              button_
                [ onClick $ setScreenAction Donate,
                  Css.fullWidth
                ]
                [ text "Donate"
                ]
            ]
        }
  where
    openWidget =
      PushUpdate
        . Instant
        . PureUpdate
        $ #modelLinks
        .~ Opened
    openBrowser =
      PushUpdate
        . Instant
        . Jsm.openBrowserPage
