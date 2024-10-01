module App.Widgets.Menu
  ( menu,
  )
where

import qualified App.Misc as Misc
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
import qualified Functora.Miso.Widgets.Grid as Grid
import qualified Functora.Miso.Widgets.Select as Select
import qualified Functora.Money as Money
import qualified Text.URI as URI

menu :: Model -> [View Action]
menu st =
  [ menu_
      [ class_ "no-print",
        style_
          [ ("flex-direction", "row"),
            ("flex-wrap", "wrap")
          ]
      ]
      [ li_
          [ role_ "button",
            onClick opened
          ]
          [ text "menu"
          ],
        li_
          [ role_ "button",
            onClick . PushUpdate . Instant . ImpureUpdate $ do
              doc <- liftIO newSt
              pure $ #modelState .~ doc
          ]
          [ text "Delivery Calculator"
          ],
        li_
          [ role_ "button",
            onClick
              . PushUpdate
              . Instant
              . PureUpdate
              $ #modelFav
              .~ Opened
          ]
          [ text "favorite"
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
          [ text "print"
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
          [ text "download"
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
          [ text "share"
          ]
      ]
  ]
    <> Fav.fav st
    <> Dialog.dialog
      Dialog.defOpts
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
                      Misc.pushActionQueue st . Instant,
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
                      Misc.pushActionQueue st . Instant,
                    Currency.argsCurrencies =
                      #modelCurrencies
                  },
              Grid.mediumCell
                [ span_ mempty [text "Exchange rate"],
                  Select.select
                    Select.defOpts
                    Select.Args
                      { Select.argsModel =
                          st,
                        Select.argsOptic =
                          #modelState . #stOnlineOrOffline,
                        Select.argsAction =
                          PushUpdate . Instant,
                        Select.argsOptions =
                          constTraversal $ enumerate @OnlineOrOffline
                      }
                ],
              Grid.mediumCell
                [ Field.ratioField
                    Field.Args
                      { Field.argsModel = st,
                        Field.argsOptic =
                          #modelState . #stExchangeRate,
                        Field.argsAction =
                          PushUpdate . Instant,
                        Field.argsEmitter =
                          Misc.pushActionQueue st . Instant
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
                    )
                ],
              Grid.mediumCell
                [ Field.dynamicField
                    Field.Args
                      { Field.argsModel = st,
                        Field.argsOptic =
                          #modelState . #stMerchantFeePercent,
                        Field.argsAction =
                          PushUpdate . Instant,
                        Field.argsEmitter =
                          Misc.pushActionQueue st . Instant
                      }
                    ( Field.defOpts
                        & #optsPlaceholder
                        .~ ("Merchant fee %" :: Unicode)
                    )
                ],
              Grid.mediumCell
                [ Field.textField
                    Field.Args
                      { Field.argsModel = st,
                        Field.argsOptic = #modelState . #stMerchantTele,
                        Field.argsAction = PushUpdate . Instant,
                        Field.argsEmitter =
                          Misc.pushActionQueue st . Instant
                      }
                    ( Field.defOpts
                        & #optsPlaceholder
                        .~ ("Merchant telegram" :: Unicode)
                    )
                ],
              Grid.mediumCell
                [ Field.textField
                    Field.Args
                      { Field.argsModel = st,
                        Field.argsOptic = #modelState . #stPreview,
                        Field.argsAction = PushUpdate . Instant,
                        Field.argsEmitter =
                          Misc.pushActionQueue st . Instant
                      }
                    ( Field.defOpts @Model @Action
                        & #optsPlaceholder
                        .~ ("QR title" :: Unicode)
                    )
                ],
              Grid.mediumCell
                [ button_
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
            ]
              <> linksWidget st
        }
  where
    opened = PushUpdate . Instant . PureUpdate $ #modelMenu .~ Opened
    screen next =
      PushUpdate
        . Instant
        . PureUpdate
        $ (& #modelMenu .~ Closed)
        . (& #modelLoading .~ isQrCode next)
        . (& #modelState . #stScreen .~ next)
    sc =
      st ^. #modelState . #stScreen

linksWidget :: Model -> [View Action]
linksWidget st =
  [ Grid.bigCell
      [ button_
          [ onClick openWidget,
            Css.fullWidth
          ]
          [ text "App"
          ]
      ]
  ]
    <> Dialog.dialog
      Dialog.defOpts
      Dialog.Args
        { Dialog.argsModel = st,
          Dialog.argsOptic = #modelLinks,
          Dialog.argsAction = PushUpdate . Instant,
          Dialog.argsContent =
            [ Grid.bigCell
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
                  text " directly."
                ],
              Grid.mediumCell
                [ button_
                    [ onClick $ openBrowser testGroupLink,
                      Css.fullWidth
                    ]
                    [ text "Join testing (closed beta)"
                    ]
                ],
              Grid.mediumCell
                [ button_
                    [ onClick $ openBrowser googlePlayLink,
                      Css.fullWidth
                    ]
                    [ text "Google Play (closed beta)"
                    ]
                ],
              Grid.mediumCell
                [ button_
                    [ onClick $ openBrowser apkLink,
                      Css.fullWidth
                    ]
                    [ text "Download APK"
                    ]
                ],
              Grid.mediumCell
                [ button_
                    [ onClick $ openBrowser sourceLink,
                      Css.fullWidth
                    ]
                    [ text "Source"
                    ]
                ],
              Grid.mediumCell
                [ button_
                    [ onClick $ openBrowser functoraLink,
                      Css.fullWidth
                    ]
                    [ text "Author"
                    ]
                ],
              Grid.mediumCell
                [ button_
                    [ onClick $ setScreenAction Donate,
                      Css.fullWidth
                    ]
                    [ text "Donate"
                    ]
                ]
            ]
        }
  where
    openWidget = PushUpdate . Instant . PureUpdate $ #modelLinks .~ Opened
    openBrowser =
      PushUpdate
        . Instant
        . Jsm.openBrowserPage
