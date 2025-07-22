module App.Widgets.Menu
  ( menu,
  )
where

import App.Types
import qualified Data.Time.Format as TF
import qualified Data.Time.LocalTime as LT
import qualified Functora.Miso.Jsm as Jsm
import Functora.Miso.Prelude
import qualified Functora.Miso.Widgets.Currency as Currency
import qualified Functora.Miso.Widgets.Dialog as Dialog
import qualified Functora.Miso.Widgets.Field as Field
import qualified Functora.Miso.Widgets.Icon as Icon
import qualified Functora.Miso.Widgets.Select as Select
import qualified Functora.Miso.Widgets.Switch as Switch

menu :: Model -> [View Action]
menu st =
  [ keyed "menu"
      $ nav_
        [ style_
            [ ("grid-template-columns", "1fr auto")
            ]
        ]
        [ label_
            [ style_
                [ ("padding-left", "1rem"),
                  ("padding-right", "1rem")
                ]
            ]
            [ text
                . from @String @Unicode
                $ TF.formatTime TF.defaultTimeLocale "%H:%M" chinaTime
                <> " in China"
            ],
          button_
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
            ]
        ]
  ]
    <> Dialog.dialog
      ( Dialog.defOpts
          { Dialog.optsTitleIcon = Just Icon.IconSettings,
            Dialog.optsKeyed = Just "menu",
            Dialog.optsTitle = Just "Settings"
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
              <> Field.ratioField
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
    fullResetOnClick :: Attribute Action
    fullResetOnClick =
      onClick . PushUpdate . ImpureUpdate $ do
        doc <- liftIO newSt
        Jsm.popupText @Unicode "Success!"
        pure $ (#modelMenu .~ Closed) . (#modelState .~ doc)
