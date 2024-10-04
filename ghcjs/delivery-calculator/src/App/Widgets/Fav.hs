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
import qualified Functora.Miso.Widgets.Dialog as Dialog
import qualified Functora.Miso.Widgets.Field as Field
import qualified Functora.Miso.Widgets.Grid as Grid

fav :: Model -> [View Action]
fav st =
  Dialog.dialog
    Dialog.defOpts
    Dialog.Args
      { Dialog.argsModel = st,
        Dialog.argsOptic = #modelFav,
        Dialog.argsAction = PushUpdate . Instant,
        Dialog.argsContent =
          [ Grid.grid mempty
              $ favItems st
              <> [ Grid.bigCell
                    $ Field.textField
                      Field.Args
                        { Field.argsModel = st,
                          Field.argsOptic = #modelState . #stFavName,
                          Field.argsAction = PushUpdate . Instant,
                          Field.argsEmitter = pushActionQueue st . Instant
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
                 ]
          ]
      }
  where
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
    [ button_
        [ onClick openAction,
          Css.fullWidth
        ]
        [ text label
        ]
    ]
  where
    openAction =
      PushUpdate
        . Instant
        $ PureAndImpureUpdate
          ( (#modelLoading .~ True)
              . (#modelFav .~ Closed)
          )
          ( do
              --
              -- TODO : Implement here pure, less costly equivalent of newModel.
              --
              next <- newModel (st ^. #modelWebOpts) (Just st) uri
              pure $ #modelState .~ modelState next
          )
