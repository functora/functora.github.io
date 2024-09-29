module App.Widgets.Fav
  ( fav,
  )
where

import qualified App.Misc as Misc
import App.Types
import App.Widgets.Templates
import qualified Data.Map as Map
import qualified Functora.Miso.Css as Css
import qualified Functora.Miso.Jsm as Jsm
import Functora.Miso.Prelude
import qualified Functora.Miso.Widgets.Field as Field
import qualified Functora.Miso.Widgets.Grid as Grid
import qualified Material.Button as Button
import qualified Material.Dialog as Dialog
import qualified Material.Theme as Theme

fav :: Model -> [View Action]
fav st =
  if st ^. #modelFav == Closed
    then mempty
    else
      [ Dialog.dialog
          ( Dialog.config
              & Dialog.setOnClose closeAction
              & Dialog.setOpen (Opened == st ^. #modelFav)
          )
          ( Dialog.dialogContent
              Nothing
              [ Grid.grid
                  mempty
                  $ favItems st
                  <> [ Grid.bigCell
                        [ Field.textField
                            Field.Args
                              { Field.argsModel = st,
                                Field.argsOptic = #modelState . #stFavName,
                                Field.argsAction = PushUpdate . Instant,
                                Field.argsEmitter =
                                  Misc.pushActionQueue st . Instant
                              }
                            Field.defOpts
                              { Field.optsPlaceholder = "Name",
                                Field.optsOnKeyDownAction = onKeyDownAction,
                                Field.optsTrailingWidget =
                                  let w =
                                        Field.ActionWidget
                                          "favorite"
                                          mempty
                                          . PushUpdate
                                          $ Instant saveAction
                                   in Just
                                        $ Field.OptsWidgetPair w w,
                                Field.optsLeadingWidget =
                                  let w =
                                        Field.ActionWidget
                                          "delete_forever"
                                          mempty
                                          deleteAction
                                   in Just
                                        $ Field.OptsWidgetPair w w
                              }
                        ],
                       Grid.bigCell
                        [ Button.raised
                            ( Button.config
                                & Button.setOnClick closeAction
                                & Button.setIcon (Just "arrow_back")
                                & Button.setAttributes [Css.fullWidth]
                            )
                            "Back"
                        ]
                     ]
              ]
              mempty
          )
      ]
  where
    closeAction =
      PushUpdate
        . Instant
        . PureUpdate
        $ #modelFav
        .~ Closed
    saveAction =
      ImpureUpdate $ do
        ct <- getCurrentTime
        uri <- stUri st
        let txt = makeFavName st
        let nextFav = Fav {favUri = uri, favCreatedAt = ct}
        let nextFavName = makeFavName st
        Jsm.popupText
          $ "Saved"
          <> ( if txt == mempty
                then mempty
                else " "
             )
          <> txt
          <> "!"
        pure
          $ #modelFavMap
          . at nextFavName
          %~ (Just . maybe nextFav (& #favUri .~ uri))
    deleteAction = PushUpdate . Instant . ImpureUpdate $ do
      let txt = makeFavName st
      let nextFavName = makeFavName st
      Jsm.popupText
        $ "Removed"
        <> ( if txt == mempty
              then mempty
              else " "
           )
        <> txt
        <> "!"
      pure
        $ #modelFavMap
        . at nextFavName
        .~ Nothing
    onKeyDownAction uid code =
      if code == KeyCode 13
        then saveAction
        else Jsm.enterOrEscapeBlur uid code

makeFavName :: Model -> Unicode
makeFavName =
  toUpper
    . strip
    . (^. #modelState . #stFavName . #fieldInput . #uniqueValue)

favItems :: Model -> [View Action]
favItems st =
  fmap (uncurry $ favItem st)
    . reverse
    . sortOn (favCreatedAt . snd)
    . Map.toList
    $ st
    ^. #modelFavMap

favItem :: Model -> Unicode -> Fav -> View Action
favItem st label Fav {favUri = uri} =
  Grid.bigCell
    [ Button.raised
        ( Button.config
            & Button.setOnClick openAction
            & Button.setAttributes
              [ Css.fullWidth,
                Theme.secondaryBg
              ]
        )
        label
    ]
  where
    openAction = PushUpdate . Instant . ImpureUpdate $ do
      --
      -- TODO : Implement here pure, less costly equivalent of newModel.
      --
      next <- newModel (st ^. #modelWebOpts) (Just st) uri
      pure $ \nextSt ->
        nextSt
          & #modelFav
          .~ Closed
          & #modelLoading
          .~ True
          & #modelState
          .~ modelState next
