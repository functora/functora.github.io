module App.Widgets.Main (mainWidget) where

import qualified App.Misc as Misc
import App.Types
import qualified App.Widgets.Asset as Asset
import qualified App.Widgets.Menu as Menu
import qualified Functora.Miso.Css as Css
import qualified Functora.Miso.Jsm as Jsm
import Functora.Miso.Prelude
import qualified Functora.Miso.Widgets.BrowserLink as BrowserLink
import qualified Functora.Miso.Widgets.Field as Field
import qualified Functora.Miso.Widgets.FieldPairs as FieldPairs
import qualified Functora.Miso.Widgets.Grid as Grid
import qualified Functora.Money as Money
import Lens.Micro ((^..))
import qualified Material.Button as Button
import qualified Material.LayoutGrid as LayoutGrid
import qualified Material.Theme as Theme
import qualified Material.TopAppBar as TopAppBar
import qualified Material.Typography as Typography
import Miso hiding (at, view)

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
screenWidget st@Model {modelState = St {stScreen = QrCode sc}} =
  ( if unQrCode sc == Donate
      then mempty
      else
        Field.fieldViewer
          Field.Args
            { Field.argsModel = st,
              Field.argsOptic = #modelState . #stPreview,
              Field.argsAction = PushUpdate . Instant,
              Field.argsEmitter = Misc.pushActionQueue st . Instant
            }
  )
    <> [ Grid.bigCell
          $ FieldPairs.fieldPairsViewer
            FieldPairs.Args
              { FieldPairs.argsModel = st,
                FieldPairs.argsOptic = #modelUriViewer,
                FieldPairs.argsAction = PushUpdate . Instant,
                FieldPairs.argsEmitter = Misc.pushActionQueue st . Instant
              }
       ]
    <> [ Grid.bigCell
          [ Button.raised
              ( Button.config
                  & Button.setIcon (Just "login")
                  & Button.setAttributes [Css.fullWidth]
                  & Button.setOnClick (setScreenAction $ unQrCode sc)
              )
              "Open"
          ]
       ]
screenWidget st@Model {modelState = St {stScreen = Donate}} =
  FieldPairs.fieldPairsViewer
    FieldPairs.Args
      { FieldPairs.argsModel = st,
        FieldPairs.argsOptic = #modelDonateViewer,
        FieldPairs.argsAction = PushUpdate . Instant,
        FieldPairs.argsEmitter = Misc.pushActionQueue st . Instant
      }
    <> [ Grid.bigCell
          [ Button.raised
              ( Button.config
                  & Button.setIcon (Just "login")
                  & Button.setAttributes [Css.fullWidth]
                  & Button.setOnClick (setScreenAction Main)
              )
              "Open"
          ]
       ]
screenWidget st@Model {modelState = St {stScreen = Main}} =
  ( if null assets
      then mempty
      else buttons
  )
    <> Asset.assetsViewer st
    <> totalViewer st
    <> buttons
  where
    assets :: [View Action]
    assets = Asset.assetsViewer st
    buttons :: [View Action]
    buttons =
      [ Grid.mediumCell
          [ Button.raised
              ( Button.config
                  & Button.setIcon (Just "add_box")
                  & Button.setAttributes [Css.fullWidth]
                  & Button.setOnClick
                    ( PushUpdate . Instant . ImpureUpdate $ do
                        asset <- newAsset
                        pure $ #modelState . #stAssets %~ flip snoc asset
                    )
              )
              "Add item"
          ],
        Grid.mediumCell
          [ Button.raised
              ( Button.config
                  & Button.setIcon (Just "send")
                  & Button.setAttributes [Css.fullWidth]
                  & Button.setOnClick
                    ( PushUpdate
                        . Instant
                        . either impureThrow Jsm.openBrowserPage
                        $ stTeleUri st
                    )
              )
              "Order via Telegram"
          ]
      ]

totalViewer :: Model -> [View Action]
totalViewer st =
  if base == 0
    then mempty
    else
      [ h1_ mempty [text "Total"]
      ]
        <> FieldPairs.fieldPairsViewer
          FieldPairs.Args
            { FieldPairs.argsModel = st,
              FieldPairs.argsOptic =
                constTraversal
                  [ newFieldPairId ("Subtotal " <> baseCur)
                      . DynamicFieldText
                      $ inspectRatioDef base,
                    newFieldPairId ("Subtotal " <> quoteCur)
                      . DynamicFieldText
                      $ inspectRatioDef quote,
                    FieldPair (newTextFieldId "Fee %")
                      $ uniqueToIdentity fee
                      & #fieldOpts
                      . #fieldOptsQrState
                      .~ Nothing,
                    newFieldPairId ("Total " <> quoteCur)
                      . DynamicFieldText
                      . inspectRatioDef
                      . foldField quote
                      $ fee
                  ],
              FieldPairs.argsAction = PushUpdate . Instant,
              FieldPairs.argsEmitter = Misc.pushActionQueue st . Instant
            }
  where
    fee = st ^. #modelState . #stMerchantFeePercent
    rate = st ^. #modelState . #stExchangeRate . #fieldOutput
    base =
      foldl
        ( \acc fps ->
            if any
              ((== FieldTypeNumber) . (^. #fieldPairValue . #fieldType))
              fps
              then acc + foldl foldFieldPair 1 fps
              else acc
        )
        0
        ( st
            ^.. #modelState
              . #stAssets
              . each
              . #assetFieldPairs
        )
    quote =
      rate * base
    baseCur =
      st
        ^. #modelState
        . #stAssetCurrency
        . #currencyOutput
        . #currencyInfoCode
        . to Money.inspectCurrencyCode
        . to toUpper
    quoteCur =
      st
        ^. #modelState
        . #stMerchantCurrency
        . #currencyOutput
        . #currencyInfoCode
        . to Money.inspectCurrencyCode
        . to toUpper

foldField :: Rational -> Field DynamicField f -> Rational
foldField acc Field {fieldType = typ, fieldOutput = out} =
  case out of
    DynamicFieldNumber x
      | typ == FieldTypeNumber ->
          acc * x
    DynamicFieldNumber x
      | typ == FieldTypePercent ->
          acc * (1 + (x / 100))
    _ ->
      acc

foldFieldPair :: Rational -> FieldPair DynamicField f -> Rational
foldFieldPair acc =
  foldField acc . fieldPairValue

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
          { BrowserLink.argsLink = functoraLink,
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
