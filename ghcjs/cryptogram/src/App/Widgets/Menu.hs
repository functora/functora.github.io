module App.Widgets.Menu
  ( menu,
  )
where

import App.Types
import qualified Functora.Miso.Jsm as Jsm
import Functora.Miso.Prelude
import qualified Functora.Miso.Widgets.Dialog as Dialog
import qualified Functora.Miso.Widgets.Field as Field
import qualified Functora.Miso.Widgets.Icon as Icon
import qualified Functora.Miso.Widgets.Select as Select
import qualified Functora.Miso.Widgets.Switch as Switch

menu :: Model -> [View Action]
menu st =
  [ keyed "menu"
      $ nav_
        [ style_
            [ ("grid-template-columns", "1fr auto")
            ]
        ]
        [ label_
            [ style_
                [ ("padding-left", "1rem"),
                  ("padding-right", "1rem")
                ]
            ]
            [ text "Tada!"
            ],
          button_
            [ role_ "button",
              style_
                [ ("min-width", "0")
                ],
              onClick
                . PushUpdate
                . PureUpdate
                $ #modelMenu
                .~ Opened
            ]
            [ icon Icon.IconMenu
            ]
        ]
  ]
    <> Dialog.dialog
      ( Dialog.defOpts
          { Dialog.optsTitleIcon = Just Icon.IconSettings,
            Dialog.optsKeyed = Just "menu",
            Dialog.optsTitle = Just "Settings"
          }
      )
      Dialog.Args
        { Dialog.argsModel = st,
          Dialog.argsOptic = #modelMenu,
          Dialog.argsAction = PushUpdate,
          Dialog.argsContent =
            [ Switch.switch
                ( Switch.defOpts
                    & #optsLabel
                    ?~ "Enable theme"
                )
                Switch.Args
                  { Switch.argsModel = st,
                    Switch.argsOptic = #modelState . #stEnableTheme,
                    Switch.argsAction = PushUpdate
                  }
            ]
              <> ( if not (st ^. #modelState . #stEnableTheme)
                    then mempty
                    else
                      [ Select.select
                          ( Select.defOpts
                              & #optsLabel
                              .~ Just "Theme"
                          )
                          Select.Args
                            { Select.argsModel = st,
                              Select.argsOptic = #modelState . #stTheme,
                              Select.argsAction = PushUpdate,
                              Select.argsOptions =
                                constTraversal $ enumerate @Theme
                            }
                      ]
                 )
              <> [ Field.labeled
                    "Full reset"
                    mempty
                    [ button_
                        [type_ "reset", fullResetOnClick]
                        [icon Icon.IconDelete, text " Full reset"]
                    ]
                 ]
        }
  where
    fullResetOnClick :: Attribute Action
    fullResetOnClick =
      onClick . PushUpdate . ImpureUpdate $ do
        doc <- liftIO newSt
        Jsm.popupText @Unicode "Success!"
        pure $ (#modelMenu .~ Closed) . (#modelState .~ doc)
