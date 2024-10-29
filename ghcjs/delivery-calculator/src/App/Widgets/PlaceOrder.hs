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
            . Instant
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
              Dialog.argsAction = PushUpdate . Instant,
              Dialog.argsContent =
                [ button_
                    [ onClick
                        . either impureThrow openBrowser
                        $ teleUri st
                    ]
                    [ icon Icon.IconTelegram,
                      text " Telegram"
                    ],
                  button_
                    [ onClick
                        . either impureThrow openBrowser
                        $ whatsUri st
                    ]
                    [ icon Icon.IconWhatsApp,
                      text " WhatsApp"
                    ],
                  button_
                    [ onClick
                        . either impureThrow openBrowser
                        $ emailUri st
                    ]
                    [ icon Icon.IconEmail,
                      text " Email"
                    ],
                  button_
                    [ onClick . PushUpdate . Instant . EffectUpdate $ do
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
    . Instant
    . EffectUpdate
    . Jsm.openBrowserPage

teleUri :: (MonadThrow m) => Model -> m URI
teleUri st = do
  base <- URI.mkURI "https://t.me"
  user <-
    URI.mkPathPiece
      . from @Unicode @Text
      $ st
      ^. #modelState
      . #stMerchantTele
      . #fieldOutput
  key <- URI.mkQueryKey "text"
  val <- URI.mkQueryValue placeOrderMessage
  pure
    $ base
      { URI.uriPath = Just (False, [user]),
        URI.uriQuery = [URI.QueryParam key val]
      }

whatsUri :: (MonadThrow m) => Model -> m URI
whatsUri st = do
  base <- URI.mkURI "https://wa.me"
  user <-
    URI.mkPathPiece
      . from @Unicode @Text
      $ st
      ^. #modelState
      . #stMerchantWhats
      . #fieldOutput
  key <- URI.mkQueryKey "text"
  val <- URI.mkQueryValue placeOrderMessage
  pure
    $ base
      { URI.uriPath = Just (False, [user]),
        URI.uriQuery = [URI.QueryParam key val]
      }

emailUri :: (MonadThrow m) => Model -> m URI
emailUri st = do
  user <-
    URI.mkPathPiece
      . from @Unicode @Text
      $ st
      ^. #modelState
      . #stMerchantEmail
      . #fieldOutput
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
