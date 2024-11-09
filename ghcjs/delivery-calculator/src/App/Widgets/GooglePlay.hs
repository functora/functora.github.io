module App.Widgets.GooglePlay (googlePlay) where

import App.Types
import qualified App.Widgets.Donate as Donate
import qualified Functora.Miso.Jsm as Jsm
import Functora.Miso.Prelude
import qualified Functora.Miso.Widgets.BrowserLink as BrowserLink
import qualified Functora.Miso.Widgets.Dialog as Dialog
import qualified Functora.Miso.Widgets.Icon as Icon

googlePlay :: Model -> TopOrBottom -> [View Action]
googlePlay st = \case
  Top -> if noitem then widget else mempty
  Bottom -> if noitem then mempty else widget
  where
    noitem = null $ st ^. #modelState . #stAssets
    widget =
      [ button_
          [ onClick openWidget
          ]
          [ icon Icon.IconGooglePlay,
            text " Google Play"
          ]
      ]
        <> Dialog.dialog
          ( Dialog.defOpts
              & #optsTitle
              .~ Just ("Google Play" :: Unicode)
              & #optsFlexCol
              .~ False
              & #optsTitleIcon
              .~ Just Icon.IconGooglePlay
          )
          Dialog.Args
            { Dialog.argsModel = st,
              Dialog.argsOptic = #modelAppLinks,
              Dialog.argsAction = PushUpdate,
              Dialog.argsContent =
                [ p_
                    [ style_
                        [ ("text-align", "start")
                        ]
                    ]
                    [ text
                        "The Android app is in closed beta. To install it, join the ",
                      BrowserLink.browserLink
                        BrowserLink.Args
                          { BrowserLink.argsLink = testGroupLink,
                            BrowserLink.argsLabel = "closed beta",
                            BrowserLink.argsAction = PushUpdate
                          },
                      text " group and then install the app from ",
                      BrowserLink.browserLink
                        BrowserLink.Args
                          { BrowserLink.argsLink = mkGooglePlayLink st,
                            BrowserLink.argsLabel = "Google Play",
                            BrowserLink.argsAction = PushUpdate
                          },
                      text ", or download the ",
                      BrowserLink.browserLink
                        BrowserLink.Args
                          { BrowserLink.argsLink = apkLink,
                            BrowserLink.argsLabel = "APK file",
                            BrowserLink.argsAction = PushUpdate
                          },
                      text " directly."
                    ],
                  button_
                    [ onClick $ openBrowser testGroupLink
                    ]
                    [ icon Icon.IconGooglePlay,
                      text " Join testing (closed beta)"
                    ],
                  button_
                    [ onClick . openBrowser $ mkGooglePlayLink st
                    ]
                    [ icon Icon.IconGooglePlay,
                      text " Google Play (closed beta)"
                    ],
                  button_
                    [ onClick $ openBrowser apkLink
                    ]
                    [ icon Icon.IconAndroid,
                      text " Download APK"
                    ],
                  button_
                    [ onClick $ openBrowser sourceLink
                    ]
                    [ icon Icon.IconGit,
                      text " Source"
                    ],
                  button_
                    [ onClick $ openBrowser functoraLink
                    ]
                    [ icon Icon.IconUser,
                      text " Author"
                    ]
                ]
                  <> Donate.donate st
            }
    openWidget =
      PushUpdate
        . PureUpdate
        $ #modelAppLinks
        .~ Opened
    openBrowser =
      PushUpdate
        . EffectUpdate
        . Jsm.openBrowserPage
