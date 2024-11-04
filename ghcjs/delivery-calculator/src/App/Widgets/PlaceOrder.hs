module App.Widgets.PlaceOrder (placeOrder) where

import qualified App.Jsm as Jsm
import App.Types
import qualified App.Xlsx as Xlsx
import qualified Data.ByteString.Lazy as BL
import qualified Functora.Miso.Jsm as Jsm
import Functora.Miso.Prelude
import qualified Functora.Miso.Widgets.Dialog as Dialog
import qualified Functora.Miso.Widgets.Icon as Icon
import qualified Text.URI as URI

placeOrder :: Model -> [View Action]
placeOrder st =
  if null $ st ^. #modelState . #stAssets
    then mempty
    else
      button_
        [ onClick
            . PushUpdate
            . PureUpdate
            $ #modelPlaceOrder
            .~ Opened
        ]
        [ icon Icon.IconShopping,
          text " Place Order"
        ]
        : Dialog.dialog
          Dialog.defOpts
            { Dialog.optsTitle = Just ("Place order" :: Unicode),
              Dialog.optsFlexCol = False,
              Dialog.optsTitleIcon = Just Icon.IconShopping
            }
          Dialog.Args
            { Dialog.argsModel = st,
              Dialog.argsOptic = #modelPlaceOrder,
              Dialog.argsAction = PushUpdate,
              Dialog.argsContent =
                teleBtn st
                  <> whatsBtn st
                  <> emailBtn st
                  <> [ button_
                        [ onClick . PushUpdate . EffectUpdate $ do
                            let doc = st ^. #modelState
                            imgs <- Jsm.fetchBlobUris doc
                            file <- Xlsx.xlsxFile
                            Jsm.saveFileShare file Xlsx.xlsxMime
                              . from @BL.ByteString @ByteString
                              $ Xlsx.newXlsx doc imgs
                        ]
                        [ icon Icon.IconDownload,
                          text " Share Excel file"
                        ]
                     ]
            }

openBrowser :: URI -> Action
openBrowser =
  PushUpdate
    . EffectUpdate
    . Jsm.openBrowserPage

teleBtn :: Model -> [View Action]
teleBtn st =
  case st ^. #modelState . #stMerchantTele . #fieldOutput of
    user | null user -> mempty
    user ->
      [ button_
          [ onClick
              . either impureThrow openBrowser
              $ teleUri user
          ]
          [ icon Icon.IconTelegram,
            text " Telegram"
          ]
      ]

teleUri :: (MonadThrow m) => Unicode -> m URI
teleUri raw = do
  base <- URI.mkURI "https://t.me"
  user <- URI.mkPathPiece $ from @Unicode @Text raw
  key <- URI.mkQueryKey "text"
  val <- URI.mkQueryValue placeOrderMessage
  pure
    $ base
      { URI.uriPath = Just (False, [user]),
        URI.uriQuery = [URI.QueryParam key val]
      }

whatsBtn :: Model -> [View Action]
whatsBtn st =
  case st ^. #modelState . #stMerchantWhats . #fieldOutput of
    user | null user -> mempty
    user ->
      [ button_
          [ onClick
              . either impureThrow openBrowser
              $ whatsUri user
          ]
          [ icon Icon.IconWhatsApp,
            text " WhatsApp"
          ]
      ]

whatsUri :: (MonadThrow m) => Unicode -> m URI
whatsUri raw = do
  base <- URI.mkURI "https://wa.me"
  user <- URI.mkPathPiece $ from @Unicode @Text raw
  key <- URI.mkQueryKey "text"
  val <- URI.mkQueryValue placeOrderMessage
  pure
    $ base
      { URI.uriPath = Just (False, [user]),
        URI.uriQuery = [URI.QueryParam key val]
      }

emailBtn :: Model -> [View Action]
emailBtn st =
  case st ^. #modelState . #stMerchantEmail . #fieldOutput of
    user | null user -> mempty
    user ->
      [ button_
          [ onClick
              . either impureThrow openBrowser
              $ emailUri user
          ]
          [ icon Icon.IconEmail,
            text " Email"
          ]
      ]

emailUri :: (MonadThrow m) => Unicode -> m URI
emailUri raw = do
  user <- URI.mkPathPiece $ from @Unicode @Text raw
  base <- URI.mkURI $ "mailto:" <> URI.unRText user
  subjKey <- URI.mkQueryKey "subject"
  subjVal <- URI.mkQueryValue "Delivery Calculator"
  bodyKey <- URI.mkQueryKey "body"
  bodyVal <- URI.mkQueryValue placeOrderMessage
  pure
    $ base
      { URI.uriQuery =
          [ URI.QueryParam subjKey subjVal,
            URI.QueryParam bodyKey bodyVal
          ]
      }

placeOrderMessage :: Text
placeOrderMessage =
  "Hello, I have a question about delivery. I will share the Excel file in the next message."
