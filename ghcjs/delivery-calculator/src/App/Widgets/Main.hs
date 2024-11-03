module App.Widgets.Main (mainWidget) where

import App.Types
import qualified App.Widgets.AppLinks as AppLinks
import qualified App.Widgets.Asset as Asset
import qualified App.Widgets.MarketLinks as MarketLinks
import qualified App.Widgets.Menu as Menu
import qualified App.Widgets.PlaceOrder as PlaceOrder
import qualified App.Widgets.RemoveOrder as RemoveOrder
import qualified App.Widgets.ShareApp as ShareApp
import Functora.Miso.Prelude
import qualified Functora.Miso.Widgets.BrowserLink as BrowserLink
import qualified Functora.Miso.Widgets.FieldPairs as FieldPairs
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
          . Flex.flexRowCenter
            footer_
            ( <>
                [ style_
                    [ ("text-align", "center"),
                      ("margin-bottom", "1rem")
                    ]
                ]
            )
          $ tosWidget
          : br_ mempty
          : MarketLinks.marketLinks st
       ]
    <> ( if not $ st ^. #modelLoading
          then mempty
          else Spinner.spinner
       )

screenWidget :: Model -> [View Action]
screenWidget st@Model {modelState = St {stScreen = QrCode sc}} =
  [ Flex.flexCol
      main_
      ( <>
          [ style_ [("align-items", "center")]
          ]
      )
      $ fmap
        ( appendAttrs
            [ style_
                [ ("display", "flex"),
                  ("align-items", "center"),
                  ("flex-direction", "column")
                ]
            ]
        )
        ( FieldPairs.fieldPairsViewer
            FieldPairs.defOpts
            FieldPairs.Args
              { FieldPairs.argsModel = st,
                FieldPairs.argsOptic = #modelUriViewer,
                FieldPairs.argsAction = PushUpdate,
                FieldPairs.argsEmitter = emitter st
              }
        )
      <> [ button_
            [ onClick . setScreenAction $ unQrCode sc
            ]
            [ icon Icon.IconOpen,
              text " Delivery Calculator"
            ]
         ]
  ]
screenWidget st@Model {modelState = St {stScreen = Donate}} =
  FieldPairs.fieldPairsViewer
    FieldPairs.defOpts
    FieldPairs.Args
      { FieldPairs.argsModel = st,
        FieldPairs.argsOptic = #modelDonateViewer,
        FieldPairs.argsAction = PushUpdate,
        FieldPairs.argsEmitter = emitter st
      }
    <> [ button_
          [ onClick $ setScreenAction Main
          ]
          [ text "Open"
          ]
       ]
screenWidget st@Model {modelState = St {stScreen = Main}} =
  ( if null assets
      then mempty
      else buttons
  )
    <> [ Flex.flexCol
          main_
          ( <>
              [ style_
                  [ ("flex-direction", "column-reverse")
                  ]
              ]
          )
          $ totalViewer st
          <> Asset.assetsViewer st
       ]
    <> buttons
  where
    assets :: [View Action]
    assets = Asset.assetsViewer st
    buttons :: [View Action]
    buttons =
      singleton
        . Flex.flexRowCenter
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
        $ ( button_
              [ onClick . PushUpdate . ImpureUpdate $ do
                  asset <- newAsset
                  pure
                    $ #modelState
                    . #stAssets
                    %~ (<> [asset])
              ]
              [ icon Icon.IconAdd,
                text " Add item"
              ]
          )
        : ShareApp.shareApp st
          <> AppLinks.appLinks st
          <> RemoveOrder.removeOrder st
          <> PlaceOrder.placeOrder st

totalViewer :: Model -> [View Action]
totalViewer st =
  if null total
    then mempty
    else
      singleton
        . keyed "total"
        . fieldset_ mempty
        $ (legend_ mempty [text "Total"])
        : FieldPairs.fieldPairsViewer
          FieldPairs.defOpts
          FieldPairs.Args
            { FieldPairs.argsModel = st,
              FieldPairs.argsOptic = constTraversal total,
              FieldPairs.argsAction = PushUpdate,
              FieldPairs.argsEmitter = emitter st
            }
  where
    total = newTotal $ modelState st

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
