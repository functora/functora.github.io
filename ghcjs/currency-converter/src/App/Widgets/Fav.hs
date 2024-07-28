module App.Widgets.Fav
  ( fav,
  )
where

import qualified App.Misc as Misc
import App.Types
import qualified App.Widgets.Cell as Cell
import qualified App.Widgets.Field as Field
import App.Widgets.Templates
import qualified Data.Map as Map
import Functora.Miso.Prelude
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
              [ Cell.grid
                  mempty
                  $ favItems st
                  <> [ Cell.mediumCell
                        $ Field.textField
                          st
                          ( #modelState
                              . #stDoc
                              . #stDocPreFavName
                          )
                          ( Field.defOpts
                              & #optsPlaceholder
                              .~ ("Name - " <> makeFavName st)
                          ),
                       Cell.smallCell
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
                       Cell.smallCell
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
                       Cell.bigCell
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
      pure . ChanItem 0 $ \nextSt ->
        let uri =
              either impureThrow id
                . URI.mkURI
                $ shareLink (nextSt ^. #modelState . #stScreen) nextSt
            nextFav = do
              Fav
                { favUri = uri,
                  favCreatedAt = ct
                }
            nextFavName =
              makeFavName nextSt
         in nextSt
              & ( Misc.textPopupPure
                    $ "Saved "
                    <> nextFavName
                    <> "!"
                )
              & #modelFavMap
              . at nextFavName
              %~ ( Just
                    . maybe nextFav (& #favUri .~ uri)
                 )
    deleteAction = pureUpdate 0 $ \nextSt ->
      let nextFavName = makeFavName nextSt
       in nextSt
            & ( Misc.textPopupPure
                  $ "Removed "
                  <> nextFavName
                  <> "!"
              )
            & #modelFavMap
            . at nextFavName
            .~ Nothing

makeFavName :: Model -> MisoString
makeFavName st =
  preFavName
    <> ( if preFavName == mempty
          then mempty
          else " "
       )
    <> getCode #stConvTopMoney
    <> "/"
    <> getCode #stConvBottomMoney
  where
    preFavName =
      st
        ^. #modelState
        . #stDoc
        . #stDocPreFavName
        . #fieldOutput
    getCode optic =
      st
        ^. #modelState
        . #stDoc
        . #stDocConv
        . cloneLens optic
        . #moneyCurrency
        . #currencyOutput
        . #currencyInfoCode
        . #unCurrencyCode
        . to toMisoString

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
  Cell.bigCell
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
