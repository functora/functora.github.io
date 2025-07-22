module App.Widgets.MarketLinks (marketLinks) where

import App.Types
import qualified Functora.Miso.Jsm as Jsm
import Functora.Miso.Prelude
import qualified Functora.Miso.Widgets.Dialog as Dialog
import qualified Functora.Miso.Widgets.Icon as Icon

marketLinks :: Model -> [View Action]
marketLinks st =
  Dialog.dialog
    Dialog.defOpts
      { Dialog.optsTitle = Just ("Marketplace" :: Unicode),
        Dialog.optsFlexCol = False,
        Dialog.optsTitleIcon = Just Icon.IconShopping
      }
    Dialog.Args
      { Dialog.argsModel = st,
        Dialog.argsOptic = #modelMarketLinks,
        Dialog.argsAction = PushUpdate,
        Dialog.argsContent =
          [ button_ [onClick $ openBrowser alibabaLink] [text "1688"],
            button_ [onClick $ openBrowser alibabaLink] [text "Alibaba"],
            button_ [onClick $ openBrowser poizonLink] [text "Dewu"],
            button_ [onClick $ openBrowser poizonLink] [text "Poizon"],
            button_ [onClick $ openBrowser taobaoLink] [text "Taobao"],
            button_ [onClick $ openBrowser tmallLink] [text "Tmall"]
          ]
      }
  where
    openBrowser link =
      PushUpdate
        $ PureAndEffectUpdate
          (#modelMarketLinks .~ Closed)
          (Jsm.openBrowserPage link)

taobaoLink :: URI
taobaoLink =
  either impureThrow id
    $ mkURI "https://www.taobao.com/"

poizonLink :: URI
poizonLink =
  either impureThrow id
    $ mkURI "https://dewu.com/"

alibabaLink :: URI
alibabaLink =
  either impureThrow id
    $ mkURI "https://www.1688.com/"

tmallLink :: URI
tmallLink =
  either impureThrow id
    $ mkURI "https://www.tmall.com/"
