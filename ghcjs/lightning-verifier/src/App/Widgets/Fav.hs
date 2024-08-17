module App.Widgets.Fav
  ( fav,
  )
where

import App.Types
import App.Widgets.Templates
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Functora.Miso.Css as Css
import qualified Functora.Miso.Jsm as Jsm
import Functora.Miso.Prelude
import qualified Functora.Miso.Widgets.Field as Field
import qualified Functora.Miso.Widgets.Grid as Grid
import qualified Material.Button as Button
import qualified Material.Dialog as Dialog
import qualified Material.Theme as Theme
import qualified Text.URI as URI

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
                  <> [ Grid.mediumCell
                        $ Field.textField
                          Field.Args
                            { Field.argsModel = st,
                              Field.argsOptic = #modelFavName,
                              Field.argsAction = PushUpdate . Instant
                            }
                          ( Field.defOpts
                              & #optsPlaceholder
                              .~ ( let name = makeFavName st
                                    in "Name"
                                        <> ( if name == mempty
                                              then mempty
                                              else " - "
                                           )
                                        <> name
                                 )
                          ),
                       Grid.smallCell
                        $ Button.raised
                          ( Button.config
                              & Button.setOnClick saveAction
                              & Button.setIcon (Just "favorite")
                              & Button.setAttributes
                                [ Theme.secondaryBg,
                                  Css.fullWidth
                                ]
                          )
                          "Save",
                       Grid.smallCell
                        $ Button.raised
                          ( Button.config
                              & Button.setOnClick deleteAction
                              & Button.setIcon (Just "delete_forever")
                              & Button.setAttributes
                                [ Theme.secondaryBg,
                                  Css.fullWidth
                                ]
                          )
                          "Delete",
                       Grid.bigCell
                        $ Button.raised
                          ( Button.config
                              & Button.setOnClick closeAction
                              & Button.setIcon (Just "arrow_back")
                              & Button.setAttributes [Css.fullWidth]
                          )
                          "Back"
                     ]
              ]
              mempty
          )
      ]
  where
    closeAction = PushUpdate . Instant $ pure . (& #modelFav .~ Closed)
    saveAction = PushUpdate . Instant $ \nextSt -> do
      ct <- getCurrentTime
      let txt = makeFavName st
      let uri = either impureThrow id . URI.mkURI $ shareLink nextSt
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

makeFavName :: Model -> MisoString
makeFavName st =
  toMisoString
    . T.toUpper
    . T.strip
    $ fromMisoString preFavName
  where
    preFavName =
      st ^. #modelFavName . #fieldInput . #uniqueValue

favItems :: Model -> [View Action]
favItems st =
  fmap (uncurry $ favItem st)
    . reverse
    . sortOn (favCreatedAt . snd)
    . Map.toList
    $ st
    ^. #modelFavMap

favItem :: Model -> MisoString -> Fav -> View Action
favItem st label Fav {favUri = uri} =
  Grid.bigCell
    $ Button.raised
      ( Button.config
          & Button.setOnClick openAction
          & Button.setAttributes [Css.fullWidth]
      )
      label
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
