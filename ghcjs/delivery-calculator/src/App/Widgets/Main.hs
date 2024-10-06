module App.Widgets.Main (mainWidget) where

import App.Types
import qualified App.Widgets.Asset as Asset
import qualified App.Widgets.Menu as Menu
import qualified Functora.Miso.Jsm as Jsm
import Functora.Miso.Prelude
import qualified Functora.Miso.Widgets.BrowserLink as BrowserLink
import qualified Functora.Miso.Widgets.Field as Field
import qualified Functora.Miso.Widgets.FieldPairs as FieldPairs
import qualified Functora.Miso.Widgets.Grid as Grid
import qualified Functora.Miso.Widgets.Spinner as Spinner
import qualified Functora.Money as Money
import Lens.Micro ((^..))
import Miso hiding (at, view)

mainWidget :: Model -> View Action
mainWidget st =
  section_
    [ style_
        [ ("margin", "0"),
          ("padding", "0"),
          ("width", "100%"),
          ("min-height", "100vh"),
          ("display", "flex"),
          ("flex-direction", "column"),
          ("justify-content", "space-between")
        ]
    ]
    $ [ header_ mempty $ Menu.menu st
      ]
    <> [ main_
          [style_ [("max-width", "550px")]]
          $ screenWidget st
       ]
    <> [ footer_
          [style_ [("text-align", "center")]]
          $ tosWidget
          : Menu.qrButton st
          : Menu.linksWidget st
       ]
    <> ( if not $ st ^. #modelLoading
          then mempty
          else [Spinner.spinner]
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
              Field.argsEmitter = pushActionQueue st . Instant
            }
  )
    <> [ Grid.bigCell
          $ FieldPairs.fieldPairsViewer
            FieldPairs.Args
              { FieldPairs.argsModel = st,
                FieldPairs.argsOptic = #modelUriViewer,
                FieldPairs.argsAction = PushUpdate . Instant,
                FieldPairs.argsEmitter = pushActionQueue st . Instant
              }
       ]
    <> [ Grid.bigCell
          [ button_
              [ onClick . setScreenAction $ unQrCode sc
              ]
              [ text "Open"
              ]
          ]
       ]
screenWidget st@Model {modelState = St {stScreen = Donate}} =
  FieldPairs.fieldPairsViewer
    FieldPairs.Args
      { FieldPairs.argsModel = st,
        FieldPairs.argsOptic = #modelDonateViewer,
        FieldPairs.argsAction = PushUpdate . Instant,
        FieldPairs.argsEmitter = pushActionQueue st . Instant
      }
    <> [ Grid.bigCell
          [ button_
              [ onClick $ setScreenAction Main
              ]
              [ text "Open"
              ]
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
          [ button_
              [ onClick . PushUpdate . Instant . ImpureUpdate $ do
                  asset <- newAsset
                  pure
                    $ #modelState
                    . #stAssets
                    %~ flip snoc asset
              ]
              [ text "Add item"
              ]
          ],
        Grid.mediumCell
          [ button_
              [ onClick
                  . PushUpdate
                  . Instant
                  . either impureThrow Jsm.openBrowserPage
                  $ stTeleUri st
              ]
              [ text "Order via Telegram"
              ]
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
              FieldPairs.argsEmitter = pushActionQueue st . Instant
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
  small_
    mempty
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
