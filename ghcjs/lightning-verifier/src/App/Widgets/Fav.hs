module App.Widgets.Fav
  ( fav,
  )
where

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
                                Field.argsOptic = #modelFavName,
                                Field.argsAction = PushUpdate . Instant
                              }
                            Field.defOpts
                              { Field.optsPlaceholder = "Name",
                                Field.optsFilledOrOutlined = Outlined,
                                Field.optsOnKeyDownAction = onKeyDownAction,
                                Field.optsTrailingWidget =
                                  Just
                                    . Field.ActionWidget
                                      "favorite"
                                      mempty
                                    . PushUpdate
                                    $ Instant saveAction,
                                Field.optsLeadingWidget =
                                  Just
                                    $ Field.ActionWidget
                                      "delete_forever"
                                      mempty
                                      deleteAction
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
    closeAction = PushUpdate . Instant $ pure . (& #modelFav .~ Closed)
    saveAction nextSt = do
      ct <- getCurrentTime
      uri <- stUri nextSt
      let txt = makeFavName st
      let nextFav = Fav {favUri = uri, favCreatedAt = ct}
      let nextFavName = makeFavName nextSt
      Jsm.popupText
        $ "Saved"
        <> ( if txt == mempty
              then mempty
              else " "
           )
        <> txt
        <> "!"
      pure
        $ nextSt
        & #modelFavMap
        . at nextFavName
        %~ (Just . maybe nextFav (& #favUri .~ uri))
    deleteAction = PushUpdate . Instant $ \nextSt -> do
      let txt = makeFavName st
      let nextFavName = makeFavName nextSt
      Jsm.popupText
        $ "Removed"
        <> ( if txt == mempty
              then mempty
              else " "
           )
        <> txt
        <> "!"
      pure
        $ nextSt
        & #modelFavMap
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
    . (^. #modelFavName . #fieldInput . #uniqueValue)

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
            & Button.setAttributes [Css.fullWidth]
        )
        label
    ]
  where
    openAction = PushUpdate . Instant $ \nextSt -> do
      --
      -- TODO : Implement here pure, less costly equivalent of newModel.
      --
      next <- newModel (Just st) uri
      pure
        $ nextSt
        & #modelFav
        .~ Closed
        & #modelLoading
        .~ True
        & #modelState
        .~ modelState next
