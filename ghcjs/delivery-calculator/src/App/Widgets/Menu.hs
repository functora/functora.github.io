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
import qualified Functora.Miso.Widgets.Field as Field
import qualified Functora.Miso.Widgets.Grid as Grid
import qualified Functora.Money as Money
import qualified Material.Button as Button
import qualified Material.Dialog as Dialog
import qualified Material.IconButton as IconButton
import qualified Material.Select as Select
import qualified Material.Select.Item as SelectItem
import qualified Material.Theme as Theme
import qualified Material.TopAppBar as TopAppBar
import qualified Text.URI as URI

menu :: Model -> [View Action]
menu st =
  [ TopAppBar.short
      ( TopAppBar.config
          & TopAppBar.setAttributes [class_ "no-print"]
      )
      [ TopAppBar.row
          mempty
          [ TopAppBar.section
              [ TopAppBar.alignStart
              ]
              [ IconButton.iconButton
                  ( IconButton.config
                      & IconButton.setOnClick opened
                      & IconButton.setAttributes
                        [ TopAppBar.actionItem,
                          TopAppBar.navigationIcon
                        ]
                  )
                  "menu",
                navItemLeft
                  $ a_
                    [ style_ [("cursor", "pointer")],
                      onClick . PushUpdate . Instant $ \next -> do
                        doc <- liftIO newSt
                        pure $ next & #modelState .~ doc
                    ]
                    [ text "Delivery Calculator"
                    ]
              ],
            TopAppBar.section
              [ TopAppBar.alignEnd
              ]
              [ navItemRight
                  $ IconButton.iconButton
                    ( IconButton.config
                        & IconButton.setOnClick
                          ( PushUpdate . Instant $ \next ->
                              pure
                                $ next
                                & #modelFav
                                .~ Opened
                          )
                        & IconButton.setAttributes
                          [ TopAppBar.actionItem,
                            TopAppBar.navigationIcon
                          ]
                    )
                    "favorite",
                navItemRight
                  $ IconButton.iconButton
                    ( IconButton.config
                        & IconButton.setOnClick
                          ( PushUpdate . Instant $ \next -> do
                              Jsm.printCurrentPage "delivery-calculator"
                              pure next
                          )
                        & IconButton.setAttributes
                          [ TopAppBar.actionItem,
                            TopAppBar.navigationIcon
                          ]
                    )
                    "print",
                navItemRight
                  $ IconButton.iconButton
                    ( IconButton.config
                        & IconButton.setOnClick
                          ( PushUpdate . Instant $ \next -> do
                              Jsm.saveFile
                                "delivery-calculator.xls"
                                "application/vnd.ms-excel"
                                Xlsx.newXlsx
                              pure next
                          )
                        & IconButton.setAttributes
                          [ TopAppBar.actionItem,
                            TopAppBar.navigationIcon
                          ]
                    )
                    "download",
                IconButton.iconButton
                  ( IconButton.config
                      & IconButton.setOnClick
                        ( PushUpdate
                            . Instant
                            . Jsm.shareText
                            . from @String @Unicode
                            . either impureThrow URI.renderStr
                            $ stUri st
                        )
                      & IconButton.setAttributes
                        [ TopAppBar.actionItem,
                          TopAppBar.navigationIcon
                        ]
                  )
                  "share"
              ]
          ]
      ]
  ]
    <> Fav.fav st
    <> if st ^. #modelMenu == Closed
      then mempty
      else
        [ Dialog.dialog
            ( Dialog.config
                & Dialog.setOnClose closed
                & Dialog.setOpen (Opened == st ^. #modelMenu)
            )
            ( Dialog.dialogContent
                Nothing
                [ Grid.grid
                    mempty
                    $ [ Currency.selectCurrency
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
                            }
                          Currency.defOpts
                            { Currency.optsExtraOnClick =
                                (& #modelLoading .~ True),
                              Currency.optsButtonViewer =
                                mappend "Marketplace - "
                                  . Money.inspectCurrencyCode
                                  . Money.currencyInfoCode
                            },
                        Currency.selectCurrency
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
                            }
                          Currency.defOpts
                            { Currency.optsExtraOnClick =
                                (& #modelLoading .~ True),
                              Currency.optsButtonViewer =
                                mappend "Merchant - "
                                  . Money.inspectCurrencyCode
                                  . Money.currencyInfoCode
                            },
                        let item :| items = enumerateNE @OnlineOrOffline
                         in Grid.mediumCell
                              [ Select.outlined
                                  ( Select.config
                                      & Select.setLabel
                                        ( Just "Exchange rate"
                                        )
                                      & Select.setSelected
                                        ( Just
                                            $ st
                                            ^. #modelState
                                            . #stOnlineOrOffline
                                        )
                                      & Select.setOnChange
                                        ( \x ->
                                            PushUpdate
                                              . Instant
                                              $ pure
                                              . ( &
                                                    #modelState
                                                      . #stOnlineOrOffline
                                                      .~ x
                                                )
                                        )
                                  )
                                  ( SelectItem.selectItem
                                      (SelectItem.config item)
                                      [text $ inspect item]
                                  )
                                  $ fmap
                                    ( \x ->
                                        SelectItem.selectItem
                                          (SelectItem.config x)
                                          [text $ inspect x]
                                    )
                                    items
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
                                      & #optsFilledOrOutlined
                                      .~ Outlined
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
                                  & #optsFilledOrOutlined
                                  .~ Outlined
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
                                  & #optsFilledOrOutlined
                                  .~ Outlined
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
                                  & #optsFilledOrOutlined
                                  .~ Outlined
                              )
                          ],
                        Grid.mediumCell
                          [ Button.raised
                              ( Button.config
                                  & Button.setOnClick
                                    ( screen
                                        $ if isQrCode sc
                                          then Main
                                          else QrCode sc
                                    )
                                  & Button.setIcon
                                    ( Just
                                        $ if isQrCode sc
                                          then "local_shipping"
                                          else "qr_code_2"
                                    )
                                  & Button.setAttributes
                                    [ Theme.secondaryBg,
                                      Css.fullWidth
                                    ]
                              )
                              $ if isQrCode sc
                                then "Delivery Calculator"
                                else "QR"
                          ]
                      ]
                    <> linksWidget st
                    <> [ Grid.bigCell
                          [ Button.raised
                              ( Button.config
                                  & Button.setOnClick closed
                                  & Button.setIcon (Just "arrow_back")
                                  & Button.setAttributes [Css.fullWidth]
                              )
                              "Back"
                          ]
                       ]
                ]
                mempty
            )
        ]
  where
    opened = PushUpdate . Instant $ pure . (& #modelMenu .~ Opened)
    closed = PushUpdate . Instant $ pure . (& #modelMenu .~ Closed)
    screen next =
      PushUpdate
        . Instant
        $ pure
        . (& #modelMenu .~ Closed)
        . (& #modelLoading .~ isQrCode next)
        . (& #modelState . #stScreen .~ next)
    sc =
      st ^. #modelState . #stScreen
    navItemLeft x =
      div_
        [ TopAppBar.title,
          style_
            [ ("padding-left", "14px"),
              ("padding-right", "0")
            ]
        ]
        [ x
        ]
    navItemRight x =
      div_
        [ TopAppBar.title,
          style_
            [ ("padding-left", "0"),
              ("padding-right", "14px")
            ]
        ]
        [ x
        ]

linksWidget :: Model -> [View Action]
linksWidget st =
  [ Grid.bigCell
      [ Button.raised
          ( Button.config
              & Button.setOnClick openWidget
              & Button.setIcon (Just "android")
              & Button.setAttributes
                [ Css.fullWidth,
                  Theme.secondaryBg
                ]
          )
          "App"
      ]
  ]
    <> ( if st ^. #modelLinks == Closed
          then mempty
          else
            [ Dialog.dialog
                ( Dialog.config
                    & Dialog.setOnClose closeWidget
                    & Dialog.setOpen (Opened == st ^. #modelLinks)
                )
                ( Dialog.dialogContent
                    Nothing
                    [ Grid.grid
                        mempty
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
                            [ Button.raised
                                ( Button.config
                                    & Button.setIcon (Just "android")
                                    & Button.setOnClick (openBrowser testGroupLink)
                                    & Button.setAttributes
                                      [ Theme.secondaryBg,
                                        Css.fullWidth
                                      ]
                                )
                                "Join testing (closed beta)"
                            ],
                          Grid.mediumCell
                            [ Button.raised
                                ( Button.config
                                    & Button.setIcon (Just "android")
                                    & Button.setOnClick (openBrowser googlePlayLink)
                                    & Button.setAttributes
                                      [ Theme.secondaryBg,
                                        Css.fullWidth
                                      ]
                                )
                                "Google Play (closed beta)"
                            ],
                          Grid.mediumCell
                            [ Button.raised
                                ( Button.config
                                    & Button.setIcon (Just "download")
                                    & Button.setOnClick (openBrowser apkLink)
                                    & Button.setAttributes
                                      [ Theme.secondaryBg,
                                        Css.fullWidth
                                      ]
                                )
                                "Download APK"
                            ],
                          Grid.mediumCell
                            [ Button.raised
                                ( Button.config
                                    & Button.setIcon (Just "code")
                                    & Button.setOnClick (openBrowser sourceLink)
                                    & Button.setAttributes
                                      [ Theme.secondaryBg,
                                        Css.fullWidth
                                      ]
                                )
                                "Source"
                            ],
                          Grid.mediumCell
                            [ Button.raised
                                ( Button.config
                                    & Button.setIcon (Just "person")
                                    & Button.setOnClick (openBrowser functoraLink)
                                    & Button.setAttributes
                                      [ Theme.secondaryBg,
                                        Css.fullWidth
                                      ]
                                )
                                "Author"
                            ],
                          Grid.mediumCell
                            [ Button.raised
                                ( Button.config
                                    & Button.setIcon (Just "volunteer_activism")
                                    & Button.setOnClick (setScreenAction Donate)
                                    & Button.setAttributes
                                      [ Theme.secondaryBg,
                                        Css.fullWidth
                                      ]
                                )
                                "Donate"
                            ],
                          Grid.bigCell
                            [ Button.raised
                                ( Button.config
                                    & Button.setOnClick closeWidget
                                    & Button.setIcon (Just "arrow_back")
                                    & Button.setAttributes [Css.fullWidth]
                                )
                                "Back"
                            ]
                        ]
                    ]
                    mempty
                )
            ]
       )
  where
    openWidget = PushUpdate . Instant $ pure . (& #modelLinks .~ Opened)
    closeWidget = PushUpdate . Instant $ pure . (& #modelLinks .~ Closed)
    openBrowser =
      PushUpdate
        . Instant
        . Jsm.openBrowserPage
