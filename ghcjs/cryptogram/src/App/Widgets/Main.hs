module App.Widgets.Main (mainWidget) where

import qualified App.Jsm as Jsm
import App.Types
import qualified App.Widgets.Menu as Menu
import Functora.Miso.Prelude
import qualified Functora.Miso.Widgets.BrowserLink as BrowserLink
import qualified Functora.Miso.Widgets.Flex as Flex
import qualified Functora.Miso.Widgets.Icon as Icon
import qualified Functora.Miso.Widgets.Spinner as Spinner
import Miso hiding (at, view)

mainWidget :: Model -> View Action
mainWidget st =
  keyed "main-root"
    . div_
      [ style_
          [ ("margin", "0"),
            ("padding", "0"),
            ("min-width", "100%"),
            ("max-width", "100%"),
            ("min-height", "100vh"),
            ("display", "flex"),
            ("flex-direction", "column"),
            ("justify-content", "space-between"),
            ("align-items", "center"),
            ("color", "inherit"),
            ("background-color", "inherit")
          ]
      ]
    $ Menu.menu st
    <> [ keyed "main-content"
          . Flex.flexCol main_ id
          $ screenWidget st
       ]
    <> [ keyed "main-footer"
          $ Flex.flexRowCenter
            footer_
            ( <>
                [ style_
                    [ ("text-align", "center"),
                      ("margin-bottom", "1rem")
                    ]
                ]
            )
            [tosWidget]
       ]
    <> ( if not $ st ^. #modelLoading
          then mempty
          else Spinner.spinner
       )

screenWidget :: Model -> [View Action]
screenWidget _ =
  [ Flex.flexCol
      main_
      ( <>
          [ style_
              [ ("flex-direction", "column-reverse")
              ]
          ]
      )
      mempty
  ]
    <> buttons
  where
    buttons :: [View Action]
    buttons =
      singleton
        $ Flex.flexRowCenter
          main_
          ( mappend
              [ style_
                  [ ("margin-left", "0"),
                    ("margin-right", "0"),
                    ("min-width", "100%"),
                    ("max-width", "100%")
                  ]
              ]
          )
          [ button_
              [ onClick . PushUpdate . ImpureUpdate $ do
                  doc <- liftIO newSt
                  cid <- Jsm.getChatId
                  Jsm.switchInlineQuery $ "Success " <> inspect cid
                  pure $ (#modelMenu .~ Closed) . (#modelState .~ doc)
              ]
              [ icon Icon.IconBitcoin,
                text " Donate"
              ]
          ]

tosWidget :: View Action
tosWidget =
  small_
    [ style_ [("width", "100%")]
    ]
    [ Miso.text "\169 2024 ",
      BrowserLink.browserLink
        BrowserLink.Args
          { BrowserLink.argsLink = functoraLink,
            BrowserLink.argsLabel = "Functora",
            BrowserLink.argsAction = PushUpdate
          },
      Miso.text ". All rights reserved. ",
      Miso.text "By continuing to use this software, you agree to the ",
      a_ [href_ "license.html"] [Miso.text "Terms of Service"],
      Miso.text " and ",
      a_ [href_ "privacy.html"] [Miso.text "Privacy Policy"],
      Miso.text $ ". Version " <> vsn <> "."
    ]
