module App.Widgets.Main (mainWidget) where

import qualified App.Misc as Misc
import App.Types
import qualified App.Widgets.Bolt11 as Bolt11
import qualified App.Widgets.Decrypt as Decrypt
import qualified App.Widgets.Menu as Menu
import qualified Functora.Miso.Css as Css
import qualified Functora.Miso.Jsm as Jsm
import Functora.Miso.Prelude
import qualified Functora.Miso.Widgets.BrowserLink as BrowserLink
import qualified Functora.Miso.Widgets.Field as Field
import qualified Functora.Miso.Widgets.FieldPairs as FieldPairs
import qualified Functora.Miso.Widgets.Header as Header
import qualified Functora.Miso.Widgets.Qr as Qr
import qualified Material.Button as Button
import qualified Material.LayoutGrid as LayoutGrid
import qualified Material.Theme as Theme
import qualified Material.TopAppBar as TopAppBar
import qualified Material.Typography as Typography
import Miso hiding (view)
import qualified Text.URI as URI

mainWidget :: Model -> View Action
mainWidget st =
  LayoutGrid.layoutGrid
    [ LayoutGrid.alignMiddle
    ]
    $ [ LayoutGrid.inner
          [ class_ "container",
            TopAppBar.shortFixedAdjust
          ]
          ( Menu.menu st
              <> screenWidget st
              <> [ -- LayoutGrid.cell [LayoutGrid.span12]
                   --  . (: mempty)
                   --  $ div_
                   --    mempty
                   --    [ text . inspect $ st ^. #modelFavMap
                   --    ],
                   tosWidget
                 ]
          )
      ]
    <> ( if st ^. #modelLoading
          then
            [ div_
                [ class_
                    "mdc-dialog mdc-dialog--fullscreen fullscreen-dialog mdc-dialog--open mdc-dialog-scroll-divider-footer mdc-dialog--scrollable"
                ]
                [ div_ [class_ "mdc-dialog__scrim"] mempty,
                  div_ [class_ "lds-dual-ring"] mempty
                ]
            ]
          else mempty
       )

screenWidget :: Model -> [View Action]
screenWidget st@Model {modelState = St {stCpt = Just {}}} =
  case st ^. #modelState . #stScreen of
    QrCode sc ->
      Header.headerWrapper
        ( Field.dynamicFieldViewer
            (PushUpdate . Instant)
            (st ^. #modelState . #stPre)
        )
        <> Qr.qr
          Qr.Args
            { Qr.argsValue =
                toMisoString
                  . either impureThrow URI.render
                  . stUri
                  $ st
                  & #modelState
                  . #stScreen
                  %~ unQrCode,
              Qr.argsAction =
                PushUpdate
                  . Instant
            }
          ( Qr.defOpts @Action
              & #optsExtraWidgets
              .~ [ Button.raised
                    ( Button.config
                        & Button.setIcon (Just "login")
                        & Button.setAttributes [Css.fullWidth]
                        & Button.setOnClick (setScreenAction $ unQrCode sc)
                    )
                    "Open"
                 ]
          )
    _ ->
      Decrypt.decrypt st
screenWidget st@Model {modelState = St {stScreen = QrCode sc}} =
  case stUri $ st & #modelState . #stScreen %~ unQrCode of
    Left e -> impureThrow e
    Right uri ->
      Header.headerWrapper
        ( Field.dynamicFieldViewer
            (PushUpdate . Instant)
            (st ^. #modelState . #stPre)
        )
        <> Qr.qr
          Qr.Args
            { Qr.argsValue = toMisoString $ URI.render uri,
              Qr.argsAction = PushUpdate . Instant
            }
          ( Qr.defOpts @Action
              & #optsExtraWidgets
              .~ [ Button.raised
                    ( Button.config
                        & Button.setIcon (Just "login")
                        & Button.setAttributes [Css.fullWidth]
                        & Button.setOnClick (setScreenAction $ unQrCode sc)
                    )
                    "Open"
                 ]
          )
screenWidget st@Model {modelState = St {stScreen = Converter}} =
  FieldPairs.fieldPairsViewer
    FieldPairs.Args
      { FieldPairs.argsModel = st,
        FieldPairs.argsOptic = #modelState . #stDoc . #stDocFieldPairs,
        FieldPairs.argsAction = PushUpdate . Instant
      }
    <> [ Field.textField @Model @Action
          Field.Args
            { Field.argsModel = st,
              Field.argsOptic = #modelState . #stDoc . #stDocLnInvoice,
              Field.argsAction = PushUpdate . Delayed 300
            }
          ( Field.defOpts
              { Field.optsFilledOrOutlined = Outlined,
                Field.optsPlaceholder = "Invoice",
                Field.optsLeadingWidget =
                  pasteWidget $ #modelState . #stDoc . #stDocLnInvoice
              }
          ),
         Field.textField
          Field.Args
            { Field.argsModel = st,
              Field.argsOptic = #modelState . #stDoc . #stDocLnPreimage,
              Field.argsAction = PushUpdate . Delayed 300
            }
          Field.defOpts
            { Field.optsFilledOrOutlined = Outlined,
              Field.optsPlaceholder = "Preimage",
              Field.optsLeadingWidget =
                pasteWidget $ #modelState . #stDoc . #stDocLnPreimage
            }
       ]
    <> Bolt11.bolt11 st

pasteWidget ::
  ATraversal' Model (Field MisoString Unique) ->
  Maybe (Field.OptsWidget Model Action)
pasteWidget optic =
  Just
    . Field.ActionWidget "content_paste" mempty
    . PushUpdate
    . Instant
    $ \prev -> do
      Jsm.selectClipboard $ \case
        Nothing ->
          Jsm.popupText @MisoString "Failed to paste!"
        Just clip ->
          Misc.pushActionQueue prev
            . Instant
            $ pure
            . (& cloneTraversal optic . #fieldOutput .~ clip)
            . (& cloneTraversal optic . #fieldInput . #uniqueValue .~ clip)
      pure prev

tosWidget :: View Action
tosWidget =
  LayoutGrid.cell
    [ LayoutGrid.span12,
      Typography.caption,
      Theme.textDisabledOnBackground,
      class_ "no-print",
      style_
        [ ("text-align", "center")
        ]
    ]
    [ Miso.text "\169 2024 ",
      BrowserLink.browserLink
        BrowserLink.Args
          { BrowserLink.argsLink = "https://functora.github.io/",
            BrowserLink.argsLabel = "Functora",
            BrowserLink.argsAction = PushUpdate . Instant
          },
      Miso.text ". All rights reserved. ",
      Miso.text "By continuing to use this software, you agree to the ",
      a_ [href_ "license.html"] [Miso.text "Terms of Service"],
      Miso.text " and ",
      a_ [href_ "privacy.html"] [Miso.text "Privacy Policy"],
      Miso.text $ ". Version " <> vsn <> "."
    ]