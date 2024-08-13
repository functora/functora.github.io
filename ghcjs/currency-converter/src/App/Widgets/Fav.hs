module App.Widgets.Fav
  ( fav,
  )
where

import App.Types
import App.Widgets.Templates
import qualified Data.Map as Map
import qualified Data.Text as T
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
                              Field.argsAction = pushUpdate
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
                                  class_ "fill"
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
                                  class_ "fill"
                                ]
                          )
                          "Delete",
                       Grid.bigCell
                        $ Button.raised
                          ( Button.config
                              & Button.setOnClick closeAction
                              & Button.setIcon (Just "arrow_back")
                              & Button.setAttributes [class_ "fill"]
                          )
                          "Back"
                     ]
              ]
              mempty
          )
      ]
  where
    closeAction = pureUpdate 0 (& #modelFav .~ Closed)
    saveAction = PushUpdate $ do
      ct <- getCurrentTime
      let txt = makeFavName st
      Jsm.popupText
        $ "Saved"
        <> ( if txt == mempty
              then mempty
              else " "
           )
        <> txt
        <> "!"
      pure . ChanItem 0 $ \nextSt ->
        let uri =
              either impureThrow id
                . URI.mkURI
                $ shareLink nextSt
            nextFav = do
              Fav
                { favUri = uri,
                  favCreatedAt = ct
                }
            nextFavName =
              makeFavName nextSt
         in nextSt
              & #modelFavMap
              . at nextFavName
              %~ (Just . maybe nextFav (& #favUri .~ uri))
    deleteAction = PushUpdate $ do
      let txt = makeFavName st
      Jsm.popupText
        $ "Removed"
        <> ( if txt == mempty
              then mempty
              else " "
           )
        <> txt
        <> "!"
      pure . ChanItem 0 $ \nextSt ->
        let nextFavName = makeFavName nextSt
         in nextSt
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
          & Button.setAttributes [class_ "fill"]
      )
      label
  where
    openAction = PushUpdate $ do
      --
      -- TODO : Implement here pure, less costly equivalent of newModel.
      --
      next <- newModel (st ^. #modelWebOpts) (Just st) uri
      pure
        . ChanItem 0
        $ (#modelFav .~ Closed)
        . (#modelLoading .~ True)
        . (#modelState .~ modelState next)
