module App.Widgets.Menu
  ( menu,
  )
where

import App.Types
import qualified App.Widgets.Fav as Fav
import qualified Functora.Miso.Css as Css
import qualified Functora.Miso.Jsm as Jsm
import Functora.Miso.Prelude
import qualified Functora.Miso.Widgets.BrowserLink as BrowserLink
import qualified Functora.Miso.Widgets.Field as Field
import qualified Functora.Miso.Widgets.Grid as Grid
import qualified Material.Button as Button
import qualified Material.Dialog as Dialog
import qualified Material.IconButton as IconButton
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
                    $ [ Grid.mediumCell
                          [ Button.raised
                              ( Button.config
                                  & Button.setOnClick
                                    ( screen
                                        $ if isQrCode sc
                                          then unQrCode sc
                                          else QrCode sc
                                    )
                                  & Button.setIcon
                                    ( Just
                                        $ if isQrCode sc
                                          then "bolt"
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
                          ],
                        Grid.mediumCell
                          [ Field.textField
                              Field.Args
                                { Field.argsModel = st,
                                  Field.argsOptic = #modelState . #stPreview,
                                  Field.argsAction = PushUpdate . Instant
                                }
                              ( Field.defOpts @Model @Action
                                  & #optsPlaceholder
                                  .~ ( "Preview - "
                                        <> ( st
                                              ^. #modelState
                                              . #stPreview
                                              . #fieldType
                                              . to userFieldType
                                           )
                                     )
                                  & #optsLeadingWidget
                                  .~ Just
                                    ( Field.ModalWidget
                                        $ Field.ModalMiniWidget
                                          ( #modelState
                                              . #stPreview
                                          )
                                    )
                                  & #optsFilledOrOutlined
                                  .~ Outlined
                              )
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
              & Button.setAttributes [Css.fullWidth]
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
