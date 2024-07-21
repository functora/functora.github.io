module App.Widgets.Menu
  ( menu,
  )
where

import qualified App.Misc as Misc
import App.Prelude as Prelude
import App.Types
import qualified App.Widgets.Templates as Templates
import qualified Language.Javascript.JSaddle as JS
import qualified Material.IconButton as IconButton
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
                      & IconButton.setOnClick
                        ( screen
                            $ if unQrCode sc == sc
                              then QrCode . unQrCode
                              else unQrCode
                        )
                      & IconButton.setAttributes
                        [ TopAppBar.actionItem,
                          TopAppBar.navigationIcon
                        ]
                  )
                  $ if unQrCode sc == sc
                    then "qr_code_2"
                    else "currency_exchange",
                navItemLeft
                  $ a_
                    [ style_ [("cursor", "pointer")],
                      onClick . screen $ const Converter
                    ]
                    [ text "Currency Converter"
                    ]
              ],
            TopAppBar.section
              [ TopAppBar.alignEnd
              ]
              [ navItemRight
                  $ IconButton.iconButton
                    ( IconButton.config
                        & IconButton.setOnClick
                          ( PushUpdate $ do
                              void
                                $ JS.global
                                ^. JS.js1
                                  ("printCurrentPage" :: Text)
                                  ("currency-converter" :: Text)
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
  where
    screen fun =
      PushUpdate $ do
        uri <- URI.mkURI $ shareLink (fun sc) st
        new <- Templates.newModel (st ^. #modelWebOpts) (Just st) uri
        pure . ChanItem 0 $ const new
    sc =
      fromMaybe
        (st ^. #modelState . #stScreen)
        (st ^? #modelState . #stExt . _Just . #stExtScreen)
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
        [ TopAppBar.title
        ]
        [ x
        ]
