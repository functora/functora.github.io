module App.Widgets.Menu
  ( menu,
  )
where

import App.Types
import qualified App.Widgets.Fav as Fav
import qualified App.Widgets.Templates as Templates
import qualified Functora.Miso.Css as Css
import qualified Functora.Miso.Jsm as Jsm
import Functora.Miso.Prelude
import qualified Functora.Miso.Widgets.BrowserLink as BrowserLink
import qualified Functora.Miso.Widgets.Field as Field
import qualified Functora.Miso.Widgets.FieldPairs as FieldPairs
import qualified Functora.Miso.Widgets.Grid as Grid
import qualified Language.Javascript.JSaddle as JS
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
                        doc <- liftIO newStDoc
                        pure
                          $ next
                          & #modelState
                          . #stDoc
                          .~ doc
                          & #modelState
                          . #stCpt
                          .~ Nothing
                          & #modelState
                          . #stScreen
                          .~ Converter
                    ]
                    [ text "Converter"
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
                                & #modelFavName
                                . #fieldInput
                                . #uniqueValue
                                .~ defFavName next
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
                              void
                                $ JS.global
                                ^. JS.js1
                                  ("printCurrentPage" :: MisoString)
                                  ("currency-converter" :: MisoString)
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
                            $ shareLink @MisoString st
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
                    $ [ Grid.smallCell
                          $ Button.raised
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
                                        then "currency_exchange"
                                        else "qr_code_2"
                                  )
                                & Button.setAttributes
                                  [ Theme.secondaryBg,
                                    Css.fullWidth
                                  ]
                            )
                          $ if isQrCode sc
                            then "Rates"
                            else "QR",
                        Grid.smallCell
                          $ Button.raised
                            ( Button.config
                                & Button.setDisabled disabled
                                & Button.setOnClick
                                  ( PushUpdate
                                      . Instant
                                      . Jsm.addFieldPair
                                      $ #modelState
                                      . #stDoc
                                      . #stDocFieldPairs
                                  )
                                & Button.setIcon
                                  ( Just "add_box"
                                  )
                                & Button.setAttributes
                                  [ Theme.secondaryBg,
                                    Css.fullWidth
                                  ]
                            )
                            "Note",
                        let item :| items = enumerateNE @OnlineOrOffline
                         in Grid.mediumCell
                              $ Select.outlined
                                ( Select.config
                                    & Select.setDisabled disabled
                                    & Select.setLabel
                                      ( Just "Exchange rates"
                                      )
                                    & Select.setAttributes
                                      [ class_ "fill-inner"
                                      ]
                                    & Select.setSelected
                                      ( Just
                                          $ st
                                          ^. #modelState
                                          . #stDoc
                                          . #stDocOnlineOrOffline
                                      )
                                    & Select.setOnChange
                                      ( \x ->
                                          PushUpdate
                                            . Instant
                                            $ pure
                                            . ( &
                                                  #modelState
                                                    . #stDoc
                                                    . #stDocOnlineOrOffline
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
                                items,
                        Grid.mediumCell
                          $ Field.dynamicField
                            Field.Args
                              { Field.argsModel = st,
                                Field.argsOptic = #modelState . #stPre,
                                Field.argsAction = PushUpdate . Instant
                              }
                            ( Field.defOpts @Model @Action
                                & #optsDisabled
                                .~ disabled
                                & #optsPlaceholder
                                .~ ( "Preview - "
                                      <> ( st
                                            ^. #modelState
                                            . #stPre
                                            . #fieldType
                                            . to userFieldType
                                         )
                                   )
                                & #optsLeadingWidget
                                .~ Just
                                  ( Field.ModalWidget
                                      $ Field.ModalMiniWidget
                                        ( #modelState
                                            . #stPre
                                        )
                                  )
                                & #optsFilledOrOutlined
                                .~ Outlined
                            ),
                        Grid.mediumCell
                          $ Field.passwordField
                            Field.Args
                              { Field.argsModel = st,
                                Field.argsOptic = #modelState . #stIkm,
                                Field.argsAction = PushUpdate . Instant
                              }
                            ( Field.defOpts
                                & #optsDisabled
                                .~ disabled
                                & #optsFilledOrOutlined
                                .~ Outlined
                            )
                      ]
                    <> ( if disabled
                          then mempty
                          else
                            FieldPairs.fieldPairsEditor
                              FieldPairs.Args
                                { FieldPairs.argsModel = st,
                                  FieldPairs.argsOptic =
                                    #modelState
                                      . #stDoc
                                      . #stDocFieldPairs,
                                  FieldPairs.argsAction = PushUpdate . Instant
                                }
                       )
                    <> linksWidget st
                    <> [ Grid.bigCell
                          $ Button.raised
                            ( Button.config
                                & Button.setOnClick closed
                                & Button.setIcon (Just "arrow_back")
                                & Button.setAttributes [Css.fullWidth]
                            )
                            "Back"
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
    disabled =
      isJust $ st ^. #modelState . #stCpt
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

defFavName :: Model -> MisoString
defFavName st =
  if isJust (st ^. #modelState . #stCpt)
    || (st ^. #modelState . #stIkm . #fieldOutput /= mempty)
    then mempty
    else getCode #stDocTopMoney <> "/" <> getCode #stDocBottomMoney
  where
    getCode optic =
      st
        ^. #modelState
        . #stDoc
        . cloneLens optic
        . #moneyCurrency
        . #currencyOutput
        . #currencyInfoCode
        . #unCurrencyCode
        . to toMisoString

linksWidget :: Model -> [View Action]
linksWidget st =
  [ Grid.bigCell
      $ Button.raised
        ( Button.config
            & Button.setOnClick openWidget
            & Button.setIcon (Just "android")
            & Button.setAttributes [Css.fullWidth]
        )
        "App"
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
                            $ span_
                              mempty
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
                            $ Button.raised
                              ( Button.config
                                  & Button.setIcon (Just "android")
                                  & Button.setOnClick (openBrowser testGroupLink)
                                  & Button.setAttributes
                                    [ Theme.secondaryBg,
                                      Css.fullWidth
                                    ]
                              )
                              "Join testing (closed beta)",
                          Grid.mediumCell
                            $ Button.raised
                              ( Button.config
                                  & Button.setIcon (Just "android")
                                  & Button.setOnClick (openBrowser googlePlayLink)
                                  & Button.setAttributes
                                    [ Theme.secondaryBg,
                                      Css.fullWidth
                                    ]
                              )
                              "Google Play (closed beta)",
                          Grid.mediumCell
                            $ Button.raised
                              ( Button.config
                                  & Button.setIcon (Just "download")
                                  & Button.setOnClick (openBrowser apkLink)
                                  & Button.setAttributes
                                    [ Theme.secondaryBg,
                                      Css.fullWidth
                                    ]
                              )
                              "Download APK",
                          Grid.mediumCell
                            $ Button.raised
                              ( Button.config
                                  & Button.setIcon (Just "code")
                                  & Button.setOnClick (openBrowser sourceLink)
                                  & Button.setAttributes
                                    [ Theme.secondaryBg,
                                      Css.fullWidth
                                    ]
                              )
                              "Source",
                          Grid.mediumCell
                            $ Button.raised
                              ( Button.config
                                  & Button.setIcon (Just "person")
                                  & Button.setOnClick (openBrowser functoraLink)
                                  & Button.setAttributes
                                    [ Theme.secondaryBg,
                                      Css.fullWidth
                                    ]
                              )
                              "Author",
                          Grid.mediumCell
                            $ Button.raised
                              ( Button.config
                                  & Button.setIcon (Just "volunteer_activism")
                                  & Button.setOnClick
                                    ( PushUpdate . Instant $ \next -> do
                                        doc <- liftIO Templates.newDonateDoc
                                        pure
                                          $ next
                                          & #modelMenu
                                          .~ Closed
                                          & #modelLinks
                                          .~ Closed
                                          & #modelLoading
                                          .~ True
                                          & #modelState
                                          . #stDoc
                                          .~ doc
                                    )
                                  & Button.setAttributes
                                    [ Theme.secondaryBg,
                                      Css.fullWidth
                                    ]
                              )
                              "Donate",
                          Grid.bigCell
                            $ Button.raised
                              ( Button.config
                                  & Button.setOnClick closeWidget
                                  & Button.setIcon (Just "arrow_back")
                                  & Button.setAttributes [Css.fullWidth]
                              )
                              "Back"
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
        . either impureThrow id
        . URI.mkURI
