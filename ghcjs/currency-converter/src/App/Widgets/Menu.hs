module App.Widgets.Menu
  ( menu,
  )
where

import qualified App.Misc as Misc
import App.Prelude as Prelude
import App.Types
import qualified App.Widgets.Cell as Cell
import App.Widgets.Templates (newModel)
import qualified App.Widgets.Templates as Templates
import qualified Language.Javascript.JSaddle as JS
import qualified Material.Button as Button
import qualified Material.Dialog as Dialog
import qualified Material.IconButton as IconButton
import qualified Material.Theme as Theme
import qualified Material.TopAppBar as TopAppBar
import Miso hiding (view)
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
                navItem
                  $ a_
                    [ style_ [("cursor", "pointer")],
                      onClick . PushUpdate $ do
                        uri <- URI.mkURI baseUri
                        new <-
                          newModel
                            (st ^. #modelWebOpts)
                            (Just $ st ^. #modelMarket)
                            uri
                        pure
                          . ChanItem 0
                          $ const new
                    ]
                    [ text "Currency Converter"
                    ]
              ],
            TopAppBar.section
              [ TopAppBar.alignEnd
              ]
              [ navItem
                  $ IconButton.iconButton
                    ( IconButton.config
                        & IconButton.setOnClick
                          ( PushUpdate $ do
                              void $ JS.eval @Text "window.print();"
                              pure $ ChanItem 0 id
                          )
                        & IconButton.setAttributes
                          [ TopAppBar.actionItem,
                            TopAppBar.navigationIcon
                          ]
                    )
                    "download",
                navItem
                  $ IconButton.iconButton
                    ( IconButton.config
                        & IconButton.setOnClick
                          ( screen $ QrCode . unQrCode
                          )
                        & IconButton.setAttributes
                          [ TopAppBar.actionItem,
                            TopAppBar.navigationIcon
                          ]
                    )
                    "qr_code_2",
                IconButton.iconButton
                  ( IconButton.config
                      & IconButton.setOnClick
                        ( Misc.copyIntoClipboardAction st
                            $ shareLink @Text (st ^. #modelState . #stScreen) st
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
    <> Templates.templates #modelTemplates Templates.unfilled Editor st
    <> Templates.templates #modelExamples Templates.examples Viewer st
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
                    [ Cell.mediumCell
                        $ Button.raised
                          ( Button.config
                              & Button.setOnClick (screen $ const Converter)
                              & Button.setIcon (Just "currency_exchange")
                              & Button.setAttributes
                                [ Theme.secondaryBg,
                                  class_ "fill"
                                ]
                          )
                          "Converter",
                      Cell.mediumCell
                        $ Button.raised
                          ( Button.config
                              & Button.setOnClick (screen $ const Editor)
                              & Button.setIcon (Just "build_circle")
                              & Button.setAttributes
                                [ Theme.secondaryBg,
                                  class_ "fill"
                                ]
                          )
                          "Editor",
                      Cell.mediumCell
                        $ Button.raised
                          ( Button.config
                              & Button.setOnClick (templates #modelTemplates)
                              & Button.setIcon (Just "apps")
                              & Button.setAttributes
                                [ Theme.secondaryBg,
                                  class_ "fill"
                                ]
                          )
                          "Templates",
                      Cell.mediumCell
                        $ Button.raised
                          ( Button.config
                              & Button.setOnClick (templates #modelExamples)
                              & Button.setIcon (Just "mood")
                              & Button.setAttributes
                                [ Theme.secondaryBg,
                                  class_ "fill"
                                ]
                          )
                          "Examples",
                      Cell.bigCell
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
    screen fun =
      PushUpdate $ do
        uri <- URI.mkURI $ shareLink (fun sc) st
        new <- newModel (st ^. #modelWebOpts) (Just $ st ^. #modelMarket) uri
        pure . ChanItem 0 $ const new
    sc =
      fromMaybe
        (st ^. #modelState . #stScreen)
        (st ^? #modelState . #stExt . _Just . #stExtScreen)
    templates opt =
      pureUpdate 0
        $ (opt .~ Opened)
        . (#modelMenu .~ Closed)
    navItem x =
      div_
        [ TopAppBar.title,
          style_ [("padding", "0")]
        ]
        [ x
        ]
