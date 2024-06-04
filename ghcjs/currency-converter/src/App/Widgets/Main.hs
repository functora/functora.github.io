module App.Widgets.Main (mainWidget) where

import qualified App.Misc as Misc
import App.Types
import qualified App.Widgets.Assets as Assets
import qualified App.Widgets.Currency as Currency
import qualified App.Widgets.Decrypt as Decrypt
import qualified App.Widgets.EditorSettings as EditorSettings
import qualified App.Widgets.Field as Field
import qualified App.Widgets.FieldPairs as FieldPairs
import qualified App.Widgets.Header as Header
import qualified App.Widgets.Menu as Menu
import qualified App.Widgets.PaymentMethods as PaymentMethods
import qualified App.Widgets.Qr as Qr
import qualified App.Widgets.SwapAmounts as SwapAmounts
import Functora.Money
import Functora.Prelude as Prelude
import qualified Material.Button as Button
import qualified Material.LayoutGrid as LayoutGrid
import qualified Material.Snackbar as Snackbar
import qualified Material.Theme as Theme
import qualified Material.Typography as Typography
import Miso hiding (view)
import Miso.String (ms)
import qualified Text.URI as URI

mainWidget :: Model -> View Action
mainWidget st =
  LayoutGrid.layoutGrid
    [ LayoutGrid.alignMiddle
    ]
    $ [ LayoutGrid.inner
          ( [ class_ "container"
            ]
              --
              -- NOTE : Hiding widget on the first render to avoid flickering.
              --
              <> ( if st ^. #modelHide
                    then [style_ [("display", "none")]]
                    else mempty
                 )
          )
          ( menu
              <> screenWidget st
              <> [ -- LayoutGrid.cell [LayoutGrid.span12]
                   --  . (: mempty)
                   --  $ div_
                   --    mempty
                   --    [ inspect . newIdentityState $ st ^. #modelState
                   --    ],
                   tosWidget,
                   Snackbar.snackbar (Snackbar.config Misc.textPopupClosed)
                    $ modelSnackbarQueue st
                 ]
          )
      ]
    <> ( if st ^. #modelHide
          then [div_ [class_ "lds-dual-ring"] mempty]
          else mempty
       )
  where
    sc =
      fromMaybe
        (st ^. #modelState . #stScreen)
        (st ^? #modelState . #stExt . _Just . #stExtScreen)
    menu =
      case sc of
        QrCode {} -> mempty
        _ | isJust (st ^. #modelState . #stExt) -> mempty
        _ -> Menu.menu st

screenWidget :: Model -> [View Action]
screenWidget st@Model {modelState = St {stExt = Just ext}} =
  case ext ^. #stExtScreen of
    QrCode sc ->
      Header.headerWrapper
        ( Field.dynamicFieldViewer st (ext ^. #stExtPre)
        )
        <> Qr.qr
          st
          ( either impureThrow URI.render
              . Misc.stExtUri
              $ ext
              & #stExtScreen
              %~ unQrCode
          )
          ( Qr.defOpts
              & #optsExtraWidgets
              .~ [ Button.raised
                    ( Button.config
                        & Button.setIcon (Just "login")
                        & Button.setAttributes [class_ "fill"]
                        & Button.setOnClick
                          ( Misc.setExtScreenAction $ unQrCode sc
                          )
                    )
                    "Open"
                 ]
          )
    _ ->
      Decrypt.decrypt st
screenWidget st@Model {modelState = St {stScreen = QrCode sc}} =
  case Misc.stUri $ st & #modelState . #stScreen %~ unQrCode of
    Left e -> impureThrow e
    Right uri ->
      Header.headerWrapper
        ( Field.dynamicFieldViewer st (st ^. #modelState . #stPre)
        )
        <> Qr.qr
          st
          ( URI.render uri
          )
          ( Qr.defOpts
              & #optsExtraWidgets
              .~ [ Button.raised
                    ( Button.config
                        & Button.setIcon (Just "login")
                        & Button.setAttributes [class_ "fill"]
                        & Button.setOnClick (Misc.setScreenAction $ unQrCode sc)
                    )
                    "Open"
                 ]
          )
screenWidget st@Model {modelState = St {stScreen = Converter}} =
  let amountWidget' loc =
        Field.ratioField
          st
          ( Misc.getConverterAmountOptic loc
          )
          ( Field.defOpts
              & #optsExtraOnInput
              .~ (& #modelState . #stConv . #stConvTopOrBottom .~ loc)
              & #optsPlaceholder
              .~ ( st
                    ^. cloneLens (Misc.getConverterCurrencyOptic loc)
                    . #currencyOutput
                    . to (inspectCurrencyInfo @Text)
                 )
          )
      currencyWidget' =
        Currency.selectCurrency st
          . cloneLens
          . Misc.getConverterCurrencyOptic
   in [ amountWidget' Top,
        currencyWidget' Top,
        amountWidget' Bottom,
        currencyWidget' Bottom,
        SwapAmounts.swapAmounts,
        Currency.swapCurrencies
      ]
screenWidget st@Model {modelState = St {stScreen = Editor}} =
  Header.headerEditor
    st
    ( #modelState
        . #stDoc
        . #stDocFieldPairsHeader
    )
    ( Field.defOpts
        & #optsPlaceholder
        .~ ( "Header - "
              <> ( st
                    ^. #modelState
                    . #stDoc
                    . #stDocFieldPairsHeader
                    . #fieldType
                    . to userFieldType
                 )
           )
        & #optsLeadingWidget
        .~ Just
          ( Field.ModalWidget
              $ Field.ModalMiniWidget
                ( #modelState
                    . #stDoc
                    . #stDocFieldPairsHeader
                )
          )
        & #optsTrailingWidget
        .~ Just
          ( Field.ActionWidget "add_box" [Theme.primary]
              . Misc.newFieldPairAction
              $ #modelState
              . #stDoc
              . #stDocFieldPairs
          )
    )
    <> FieldPairs.fieldPairs
      st
      ( #modelState
          . #stDoc
          . #stDocFieldPairs
      )
    <> Header.headerEditor
      st
      ( #modelState
          . #stDoc
          . #stDocAssetsHeader
      )
      ( Field.defOpts
          & #optsPlaceholder
          .~ ( "Assets - "
                <> ( st
                      ^. #modelState
                      . #stDoc
                      . #stDocAssetsHeader
                      . #fieldType
                      . to userFieldType
                   )
             )
          & #optsLeadingWidget
          .~ Just
            ( Field.ModalWidget
                $ Field.ModalMiniWidget
                  ( #modelState
                      . #stDoc
                      . #stDocAssetsHeader
                  )
            )
          & #optsTrailingWidget
          .~ Just
            ( Field.ActionWidget "add_box" [Theme.primary]
                . Misc.newAssetAction st
                $ #modelState
                . #stDoc
                . #stDocAssets
            )
      )
    <> Assets.assets
      st
      ( #modelState
          . #stDoc
          . #stDocAssets
      )
    <> Header.headerEditor
      st
      ( #modelState
          . #stDoc
          . #stDocPaymentMethodsHeader
      )
      ( Field.defOpts
          & #optsPlaceholder
          .~ ( "Payments - "
                <> ( st
                      ^. #modelState
                      . #stDoc
                      . #stDocPaymentMethodsHeader
                      . #fieldType
                      . to userFieldType
                   )
             )
          & #optsLeadingWidget
          .~ Just
            ( Field.ModalWidget
                $ Field.ModalMiniWidget
                  ( #modelState
                      . #stDoc
                      . #stDocPaymentMethodsHeader
                  )
            )
          & #optsTrailingWidget
          .~ Just
            ( Field.ActionWidget "add_box" [Theme.primary]
                . Misc.newPaymentMethodAction st
                $ #modelState
                . #stDoc
                . #stDocPaymentMethods
            )
      )
    <> PaymentMethods.paymentMethods
      st
      ( #modelState
          . #stDoc
          . #stDocPaymentMethods
      )
    <> EditorSettings.editorSettings st
screenWidget st@Model {modelState = St {stScreen = Viewer}} =
  Header.headerWrapper
    ( Field.dynamicFieldViewer
        st
        (st ^. #modelState . #stDoc . #stDocFieldPairsHeader)
    )
    <> FieldPairs.fieldPairsViewer
      st
      (st ^. #modelState . #stDoc . #stDocFieldPairs)
    <> Header.headerWrapper
      ( Field.dynamicFieldViewer
          st
          (st ^. #modelState . #stDoc . #stDocAssetsHeader)
      )
    <> Assets.assetsViewer
      st
      (st ^. #modelState . #stDoc . #stDocAssets)
    <> Header.headerWrapper
      ( Field.dynamicFieldViewer
          st
          (st ^. #modelState . #stDoc . #stDocPaymentMethodsHeader)
      )
    <> PaymentMethods.paymentMethodsViewer
      st
      (st ^. #modelState . #stDoc . #stDocPaymentMethods)

tosWidget :: View Action
tosWidget =
  LayoutGrid.cell
    [ LayoutGrid.span12,
      Typography.caption,
      style_
        [ ("text-align", "center")
        ]
    ]
    [ Miso.text "\169 2024 Functora. All rights reserved. ",
      Miso.text "By continuing to use this software, you agree to the ",
      a_ [href_ "license.html"] [Miso.text "Terms of Service"],
      Miso.text " and ",
      a_ [href_ "privacy.html"] [Miso.text "Privacy Policy"],
      Miso.text
        ". This software is 100% organic and AI-free. It is built and tested exclusively by humans. ",
      Miso.text . ms $ "Version " <> Misc.vsn <> "."
    ]
