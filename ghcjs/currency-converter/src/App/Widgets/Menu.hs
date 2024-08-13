module App.Widgets.Menu
  ( menu,
  )
where

import qualified App.Misc as Misc
import App.Types
import qualified App.Widgets.Fav as Fav
import qualified App.Widgets.Templates as Templates
import qualified Functora.Miso.Jsm as Jsm
import Functora.Miso.Prelude
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
                      onClick . PushUpdate $ do
                        doc <- liftIO newStDoc
                        pure
                          . ChanItem 0
                          $ (& #modelState . #stDoc .~ doc)
                          . (& #modelState . #stCpt .~ Nothing)
                          . (& #modelState . #stScreen .~ Converter)
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
                          ( pureUpdate 0 $ \next ->
                              next
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
                          ( PushUpdate $ do
                              void
                                $ JS.global
                                ^. JS.js1
                                  ("printCurrentPage" :: MisoString)
                                  ("currency-converter" :: MisoString)
                              pure $ ChanItem 0 id
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
                        ( pushUpdate
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
                                    class_ "fill"
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
                                  ( pushUpdate
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
                                    class_ "fill"
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
                                          pureUpdate
                                            0
                                            ( &
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
                                Field.argsAction = pushUpdate
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
                                Field.argsAction = pushUpdate
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
                                  FieldPairs.argsAction = pushUpdate
                                }
                       )
                    <> linksWidget st
                    <> [ Grid.bigCell
                          $ Button.raised
                            ( Button.config
                                & Button.setOnClick closed
                                & Button.setIcon (Just "arrow_back")
                                & Button.setAttributes [class_ "fill"]
                            )
                            "Back"
                       ]
                ]
                mempty
            )
        ]
  where
    opened = pureUpdate 0 (& #modelMenu .~ Opened)
    closed = pureUpdate 0 (& #modelMenu .~ Closed)
    screen next =
      pureUpdate 0
        $ (& #modelMenu .~ Closed)
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
            & Button.setAttributes [class_ "fill"]
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
                                Misc.browserLink testGroupLink "closed beta",
                                text " group and then install the app from ",
                                Misc.browserLink googlePlayLink "Google Play",
                                text ", or download the ",
                                Misc.browserLink apkLink "APK file",
                                text " directly."
                              ],
                          Grid.mediumCell
                            $ Button.raised
                              ( Button.config
                                  & Button.setIcon (Just "android")
                                  & Button.setOnClick (openBrowser testGroupLink)
                                  & Button.setAttributes
                                    [ Theme.secondaryBg,
                                      class_ "fill"
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
                                      class_ "fill"
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
                                      class_ "fill"
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
                                      class_ "fill"
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
                                      class_ "fill"
                                    ]
                              )
                              "Author",
                          Grid.mediumCell
                            $ Button.raised
                              ( Button.config
                                  & Button.setIcon (Just "volunteer_activism")
                                  & Button.setOnClick
                                    ( PushUpdate $ do
                                        doc <- liftIO Templates.newDonateDoc
                                        pure
                                          . ChanItem 0
                                          $ (& #modelMenu .~ Closed)
                                          . (& #modelLinks .~ Closed)
                                          . (& #modelLoading .~ True)
                                          . (& #modelState . #stDoc .~ doc)
                                    )
                                  & Button.setAttributes
                                    [ Theme.secondaryBg,
                                      class_ "fill"
                                    ]
                              )
                              "Donate",
                          Grid.bigCell
                            $ Button.raised
                              ( Button.config
                                  & Button.setOnClick closeWidget
                                  & Button.setIcon (Just "arrow_back")
                                  & Button.setAttributes [class_ "fill"]
                              )
                              "Back"
                        ]
                    ]
                    mempty
                )
            ]
       )
  where
    openWidget = pureUpdate 0 (& #modelLinks .~ Opened)
    closeWidget = pureUpdate 0 (& #modelLinks .~ Closed)
    openBrowser =
      Misc.openBrowserPageAction
        . either impureThrow id
        . URI.mkURI
