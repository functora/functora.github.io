module App.Widgets.Menu
  ( menu,
    qrButton,
    linksWidget,
  )
where

import App.Types
import qualified Data.Time.Format as TF
import qualified Data.Time.LocalTime as LT
import qualified Functora.Miso.Jsm as Jsm
import Functora.Miso.Prelude
import qualified Functora.Miso.Widgets.BrowserLink as BrowserLink
import qualified Functora.Miso.Widgets.Currency as Currency
import qualified Functora.Miso.Widgets.Dialog as Dialog
import qualified Functora.Miso.Widgets.Field as Field
import qualified Functora.Miso.Widgets.Icon as Icon
import qualified Functora.Miso.Widgets.Select as Select
import qualified Functora.Miso.Widgets.Switch as Switch
import qualified Text.URI as URI

menu :: Model -> [View Action]
menu st =
  [ keyed "menu"
      $ nav_
        [ style_
            [ ("grid-template-columns", "auto 1fr")
            ]
        ]
        [ button_
            [ role_ "button",
              style_
                [ ("min-width", "0")
                ],
              onClick
                . PushUpdate
                . PureUpdate
                $ #modelMenu
                .~ Opened
            ]
            [ icon Icon.IconMenu
            ],
          label_
            mempty
            [ text
                . from @String @Unicode
                $ TF.formatTime TF.defaultTimeLocale "%H:%M" chinaTime
                <> " in China"
            ]
        ]
  ]
    <> Dialog.dialog
      ( Dialog.defOpts
          { Dialog.optsTitleIcon = Just Icon.IconSettings,
            Dialog.optsKeyed = Just "menu",
            Dialog.optsTitle = Just "Settings",
            Dialog.optsHeaderRight =
              ( ( button_
                    [shareOnClick]
                    [Dialog.optsIcon Dialog.defOpts Icon.IconShare]
                )
                  :
              ),
            Dialog.optsFooterRight =
              ( ( button_
                    [shareOnClick]
                    [ Dialog.optsIcon Dialog.defOpts Icon.IconShare,
                      text " Share"
                    ]
                )
                  :
              )
          }
      )
      Dialog.Args
        { Dialog.argsModel = st,
          Dialog.argsOptic = #modelMenu,
          Dialog.argsAction = PushUpdate,
          Dialog.argsContent =
            Currency.selectCurrency
              Currency.defOpts
                { Currency.optsButtonLabel = Just "Marketplace currency",
                  Currency.optsExtraOnClick = #modelLoading .~ True
                }
              Currency.Args
                { Currency.argsModel = st,
                  Currency.argsOptic = #modelState . #stAssetCurrency,
                  Currency.argsAction = PushUpdate,
                  Currency.argsEmitter = emitter st,
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
                    Currency.argsAction = PushUpdate,
                    Currency.argsEmitter = emitter st,
                    Currency.argsCurrencies = #modelCurrencies
                  }
              <> [ Select.select
                    ( Select.defOpts & #optsLabel .~ Just "Exchange rate"
                    )
                    Select.Args
                      { Select.argsModel = st,
                        Select.argsOptic = #modelState . #stOnlineOrOffline,
                        Select.argsAction = PushUpdate,
                        Select.argsOptions =
                          constTraversal $ enumerate @OnlineOrOffline
                      }
                 ]
              <> Field.ratioField
                Field.Args
                  { Field.argsModel = st,
                    Field.argsOptic = #modelState . #stExchangeRate,
                    Field.argsAction = PushUpdate,
                    Field.argsEmitter = emitter st
                  }
                ( let eod =
                        if st ^. #modelState . #stOnlineOrOffline == Offline
                          then Enabled
                          else Disabled
                   in Field.defOpts @Model @Action
                        & #optsEnabledOrDisabled
                        .~ eod
                        & #optsLabel
                        .~ Just (inspectExchangeRate $ modelState st)
                )
              <> Field.dynamicField
                Field.Args
                  { Field.argsModel = st,
                    Field.argsOptic = #modelState . #stMerchantFeePercent,
                    Field.argsAction = PushUpdate,
                    Field.argsEmitter = emitter st
                  }
                ( Field.defOpts
                    & #optsLabel
                    .~ Just ("Merchant fee %" :: Unicode)
                )
              <> Field.textField
                Field.Args
                  { Field.argsModel = st,
                    Field.argsOptic = #modelState . #stMerchantTele,
                    Field.argsAction = PushUpdate,
                    Field.argsEmitter = emitter st
                  }
                ( Field.defOpts
                    & #optsLabel
                    .~ Just ("Merchant Telegram" :: Unicode)
                )
              <> Field.textField
                Field.Args
                  { Field.argsModel = st,
                    Field.argsOptic = #modelState . #stMerchantWhats,
                    Field.argsAction = PushUpdate,
                    Field.argsEmitter = emitter st
                  }
                ( Field.defOpts
                    & #optsLabel
                    .~ Just ("Merchant WhatsApp" :: Unicode)
                )
              <> Field.textField
                Field.Args
                  { Field.argsModel = st,
                    Field.argsOptic = #modelState . #stMerchantEmail,
                    Field.argsAction = PushUpdate,
                    Field.argsEmitter = emitter st
                  }
                ( Field.defOpts
                    & #optsLabel
                    .~ Just ("Merchant email" :: Unicode)
                )
              <> [ Switch.switch
                    ( Switch.defOpts
                        & #optsLabel
                        .~ Just "Enable theme"
                    )
                    Switch.Args
                      { Switch.argsModel = st,
                        Switch.argsOptic = #modelState . #stEnableTheme,
                        Switch.argsAction = PushUpdate
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
                              Select.argsAction = PushUpdate,
                              Select.argsOptions =
                                constTraversal $ enumerate @Theme
                            }
                      ]
                 )
              <> [ Field.labeled
                    "Full reset"
                    mempty
                    [ button_
                        [type_ "reset", fullResetOnClick]
                        [icon Icon.IconDelete, text " Full reset"]
                    ]
                 ]
        }
  where
    chinaTime =
      LT.utcToLocalTime chinaTimeZone $ modelTime st
    chinaTimeZone =
      LT.hoursToTimeZone 8
    shareOnClick :: Attribute Action
    shareOnClick =
      onClick
        . PushUpdate
        . Jsm.shareText
        . from @String @Unicode
        . either impureThrow URI.renderStr
        $ stUri st
    fullResetOnClick :: Attribute Action
    fullResetOnClick =
      onClick . PushUpdate . ImpureUpdate $ do
        doc <- liftIO newSt
        Jsm.popupText @Unicode "Full reset success!"
        pure $ (#modelMenu .~ Closed) . (#modelState .~ doc)

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
          Dialog.argsAction = PushUpdate,
          Dialog.argsContent =
            [ text
                "The Android app is in closed beta. To install it, join the ",
              BrowserLink.browserLink
                BrowserLink.Args
                  { BrowserLink.argsLink = testGroupLink,
                    BrowserLink.argsLabel = "closed beta",
                    BrowserLink.argsAction = PushUpdate
                  },
              text " group and then install the app from ",
              BrowserLink.browserLink
                BrowserLink.Args
                  { BrowserLink.argsLink = googlePlayLink,
                    BrowserLink.argsLabel = "Google Play",
                    BrowserLink.argsAction = PushUpdate
                  },
              text ", or download the ",
              BrowserLink.browserLink
                BrowserLink.Args
                  { BrowserLink.argsLink = apkLink,
                    BrowserLink.argsLabel = "APK file",
                    BrowserLink.argsAction = PushUpdate
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
        . PureUpdate
        $ #modelLinks
        .~ Opened
    openBrowser =
      PushUpdate
        . EffectUpdate
        . Jsm.openBrowserPage
