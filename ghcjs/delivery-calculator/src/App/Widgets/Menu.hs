module App.Widgets.Menu
  ( menu,
    qrButton,
    linksWidget,
  )
where

import qualified App.Jsm as Jsm
import App.Types
import qualified App.Widgets.Fav as Fav
import qualified App.Xlsx as Xlsx
import qualified Data.ByteString.Lazy as BL
import qualified Functora.Miso.Jsm as Jsm
import Functora.Miso.Prelude
import qualified Functora.Miso.Widgets.BrowserLink as BrowserLink
import qualified Functora.Miso.Widgets.Currency as Currency
import qualified Functora.Miso.Widgets.Dialog as Dialog
import qualified Functora.Miso.Widgets.Field as Field
import qualified Functora.Miso.Widgets.Icon as Icon
import qualified Functora.Miso.Widgets.Select as Select
import qualified Functora.Miso.Widgets.Switch as Switch
import qualified Functora.Money as Money
import qualified Text.URI as URI

menu :: Model -> [View Action]
menu st =
  [ keyed "menu"
      $ nav_
        [ style_
            [ ("grid-template-columns", "auto 1fr auto auto auto auto")
            ]
        ]
        [ button_
            [ role_ "button",
              style_
                [ ("min-width", "0")
                ],
              onClick
                . PushUpdate
                . Instant
                . PureUpdate
                $ #modelMenu
                .~ Opened
            ]
            [ icon Icon.IconMenu
            ],
          button_
            [ role_ "button",
              style_
                [ ("min-width", "0"),
                  ("justify-self", "start"),
                  ("word-break", "keep-all"),
                  ("overflow-wrap", "normal")
                ],
              onClick . PushUpdate . Instant . ImpureUpdate $ do
                doc <- liftIO newSt
                pure $ #modelState .~ doc
            ]
            [ text "Delivery Calculator"
            ],
          button_
            [ role_ "button",
              style_
                [ ("min-width", "0")
                ],
              onClick
                . PushUpdate
                . Instant
                . PureUpdate
                $ #modelFav
                .~ Opened
            ]
            [ icon Icon.IconFav
            ],
          button_
            [ role_ "button",
              style_
                [ ("min-width", "0")
                ],
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
          button_
            [ role_ "button",
              style_
                [ ("min-width", "0")
                ],
              onClick . PushUpdate . Instant . EffectUpdate $ do
                let doc = st ^. #modelState
                imgs <- Jsm.fetchBlobUris doc
                Jsm.saveFile Xlsx.xlsxFile Xlsx.xlsxMime
                  . from @BL.ByteString @ByteString
                  $ Xlsx.newXlsx doc imgs
            ]
            [ icon Icon.IconDownload
            ],
          button_
            [ role_ "button",
              style_
                [ ("min-width", "0")
                ],
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
          & #optsTitleIcon
          .~ Just Icon.IconSettings
          & #optsKeyed
          .~ Just ("menu" :: Unicode)
          & #optsTitle
          .~ Just "Settings"
      )
      Dialog.Args
        { Dialog.argsModel = st,
          Dialog.argsOptic = #modelMenu,
          Dialog.argsAction = PushUpdate . Instant,
          Dialog.argsContent =
            Currency.selectCurrency
              Currency.defOpts
                { Currency.optsButtonLabel = Just "Marketplace currency",
                  Currency.optsExtraOnClick = #modelLoading .~ True
                }
              Currency.Args
                { Currency.argsModel = st,
                  Currency.argsOptic = #modelState . #stAssetCurrency,
                  Currency.argsAction = PushUpdate . Instant,
                  Currency.argsEmitter = pushActionQueue st . Instant,
                  Currency.argsCurrencies = #modelCurrencies
                }
              <> Currency.selectCurrency
                Currency.defOpts
                  { Currency.optsButtonLabel = Just "Merchant currency",
                    Currency.optsExtraOnClick = #modelLoading .~ True
                  }
                Currency.Args
                  { Currency.argsModel = st,
                    Currency.argsOptic = #modelState . #stMerchantCurrency,
                    Currency.argsAction = PushUpdate . Instant,
                    Currency.argsEmitter = pushActionQueue st . Instant,
                    Currency.argsCurrencies = #modelCurrencies
                  }
              <> [ Select.select
                    ( Select.defOpts & #optsLabel .~ Just "Exchange rate"
                    )
                    Select.Args
                      { Select.argsModel = st,
                        Select.argsOptic = #modelState . #stOnlineOrOffline,
                        Select.argsAction = PushUpdate . Instant,
                        Select.argsOptions =
                          constTraversal $ enumerate @OnlineOrOffline
                      }
                 ]
              <> Field.ratioField
                Field.Args
                  { Field.argsModel = st,
                    Field.argsOptic = #modelState . #stExchangeRate,
                    Field.argsAction = PushUpdate . Instant,
                    Field.argsEmitter = pushActionQueue st . Instant
                  }
                ( let disabled =
                        st
                          ^. #modelState
                          . #stOnlineOrOffline
                          == Online
                   in Field.defOpts @Model @Action
                        & #optsDisabled
                        .~ disabled
                        & #optsLabel
                        .~ Just
                          ( "1 "
                              <> toUpper
                                ( Money.inspectCurrencyCode
                                    $ st
                                    ^. #modelState
                                    . #stAssetCurrency
                                    . #currencyOutput
                                    . #currencyInfoCode
                                )
                              <> " \8776 "
                              <> inspectRatioDef
                                ( st
                                    ^. #modelState
                                    . #stExchangeRate
                                    . #fieldOutput
                                )
                              <> " "
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
              <> Field.dynamicField
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
                    & #optsLabel
                    .~ Just ("Merchant fee %" :: Unicode)
                )
              <> Field.textField
                Field.Args
                  { Field.argsModel = st,
                    Field.argsOptic = #modelState . #stMerchantTele,
                    Field.argsAction = PushUpdate . Instant,
                    Field.argsEmitter = pushActionQueue st . Instant
                  }
                ( Field.defOpts
                    & #optsLabel
                    .~ Just ("Merchant telegram" :: Unicode)
                )
              <> Field.textField
                Field.Args
                  { Field.argsModel = st,
                    Field.argsOptic = #modelState . #stPreview,
                    Field.argsAction = PushUpdate . Instant,
                    Field.argsEmitter = pushActionQueue st . Instant
                  }
                ( Field.defOpts @Model @Action
                    & #optsLabel
                    .~ Just ("QR title" :: Unicode)
                )
              <> [ Switch.switch
                    ( Switch.defOpts
                        & #optsLabel
                        .~ Just "Enable theme"
                    )
                    Switch.Args
                      { Switch.argsModel = st,
                        Switch.argsOptic = #modelState . #stEnableTheme,
                        Switch.argsAction = PushUpdate . Instant
                      }
                 ]
              <> ( if not (st ^. #modelState . #stEnableTheme)
                    then mempty
                    else
                      [ Select.select
                          ( Select.defOpts
                              & #optsLabel
                              .~ Just "Theme"
                          )
                          Select.Args
                            { Select.argsModel = st,
                              Select.argsOptic = #modelState . #stTheme,
                              Select.argsAction = PushUpdate . Instant,
                              Select.argsOptions =
                                constTraversal $ enumerate @Theme
                            }
                      ]
                 )
        }

qrButton :: Model -> View Action
qrButton st =
  button_
    [ onClick
        . screen
        $ if isQrCode sc
          then Main
          else QrCode sc
    ]
    [ icon
        $ if isQrCode sc
          then Icon.IconDelivery
          else Icon.IconQrCode,
      text
        $ if isQrCode sc
          then " Calculator"
          else " QR"
    ]
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
      [ onClick openWidget
      ]
      [ icon Icon.IconGooglePlay,
        text " Google Play"
      ]
  ]
    <> Dialog.dialog
      ( Dialog.defOpts
          & #optsTitle
          .~ Just ("App" :: Unicode)
          & #optsTitleIcon
          .~ Just Icon.IconGooglePlay
      )
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
                    BrowserLink.argsAction = PushUpdate . Instant
                  },
              text " group and then install the app from ",
              BrowserLink.browserLink
                BrowserLink.Args
                  { BrowserLink.argsLink = googlePlayLink,
                    BrowserLink.argsLabel = "Google Play",
                    BrowserLink.argsAction = PushUpdate . Instant
                  },
              text ", or download the ",
              BrowserLink.browserLink
                BrowserLink.Args
                  { BrowserLink.argsLink = apkLink,
                    BrowserLink.argsLabel = "APK file",
                    BrowserLink.argsAction = PushUpdate . Instant
                  },
              text " directly.",
              button_
                [ onClick $ openBrowser testGroupLink
                ]
                [ icon Icon.IconGooglePlay,
                  text " Join testing (closed beta)"
                ],
              button_
                [ onClick $ openBrowser googlePlayLink
                ]
                [ icon Icon.IconGooglePlay,
                  text " Google Play (closed beta)"
                ],
              button_
                [ onClick $ openBrowser apkLink
                ]
                [ icon Icon.IconAndroid,
                  text " Download APK"
                ],
              button_
                [ onClick $ openBrowser sourceLink
                ]
                [ icon Icon.IconGit,
                  text " Source"
                ],
              button_
                [ onClick $ openBrowser functoraLink
                ]
                [ icon Icon.IconUser,
                  text " Author"
                ],
              button_
                [ onClick $ setScreenAction Donate
                ]
                [ icon Icon.IconBitcoin,
                  text " Donate"
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
