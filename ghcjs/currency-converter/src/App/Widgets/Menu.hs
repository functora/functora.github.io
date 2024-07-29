module App.Widgets.Menu
  ( menu,
  )
where

import qualified App.Misc as Misc
import App.Types
import qualified App.Widgets.Cell as Cell
import qualified App.Widgets.Fav as Fav
import qualified App.Widgets.Field as Field
import qualified App.Widgets.FieldPairs as FieldPairs
import Functora.Miso.Prelude
import qualified Language.Javascript.JSaddle as JS
import qualified Material.Button as Button
import qualified Material.Dialog as Dialog
import qualified Material.IconButton as IconButton
import qualified Material.Select as Select
import qualified Material.Select.Item as SelectItem
import qualified Material.Theme as Theme
import qualified Material.TopAppBar as TopAppBar

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
                          ( pureUpdate 0
                              $ (& #modelFav .~ Opened)
                              . ( &
                                    #modelState
                                      . #stDoc
                                      . #stDocPreFavName
                                      . #fieldInput
                                      . #uniqueValue
                                      .~ mempty
                                )
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
                        ( Misc.copyIntoClipboardAction st
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
                [ Cell.grid
                    mempty
                    $ [ Cell.smallCell
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
                            then "Rate"
                            else "QR",
                        Cell.smallCell
                          $ Button.raised
                            ( Button.config
                                & Button.setOnClick
                                  ( Misc.newFieldPairAction
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
                         in Cell.mediumCell
                              $ Select.outlined
                                ( Select.config
                                    & Select.setLabel
                                      ( Just "Exchange rate"
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
                        Cell.mediumCell
                          $ Field.field
                            st
                            ( #modelState . #stPre
                            )
                            ( Field.defOpts
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
                            )
                            parseDynamicField
                            inspectDynamicField,
                        Cell.mediumCell
                          $ Field.passwordField
                            st
                            ( #modelState
                                . #stIkm
                            )
                            ( Field.defOpts
                                & #optsFilledOrOutlined
                                .~ Outlined
                            )
                      ]
                    <> FieldPairs.fieldPairs
                      st
                      ( #modelState
                          . #stDoc
                          . #stDocFieldPairs
                      )
                    <> [ Cell.bigCell
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
