module App.Widgets.EditorSettings
  ( editorSettings,
  )
where

import App.Prelude as Prelude
import App.Types
import qualified App.Widgets.Cell as Cell
import qualified App.Widgets.Field as Field
import qualified App.Widgets.Header as Header
import qualified Material.Button as Button
import qualified Material.Select as Select
import qualified Material.Select.Item as SelectItem
import Miso hiding (view)

editorSettings :: Model -> [View Action]
editorSettings st =
  Header.headerEditor
    st
    ( #modelState
        . #stPre
    )
    ( Field.defOpts
        & #optsPlaceholder
        .~ ( "Preview - "
              <> ( st
                    ^. #modelState
                    . #stPre
                    . #fieldType
                    . to userFieldType
                 )
           )
        & #optsLeadingWidget
        .~ Just
          ( Field.ModalWidget
              $ Field.ModalMiniWidget (#modelState . #stPre)
          )
    )
    <> [ Cell.mediumCell
          $ Field.passwordField st (#modelState . #stIkm) Field.defOpts,
         Cell.mediumCell
          $ selectLayoutWidget st,
         Cell.bigCell
          $ Button.raised
            ( Button.config
                & Button.setIcon (Just "login")
                & Button.setAttributes [class_ "fill"]
                & Button.setOnClick (setScreenAction Viewer)
            )
            "Open"
       ]

selectLayoutWidget :: Model -> View Action
selectLayoutWidget st =
  Select.outlined
    ( Select.config
        & Select.setLabel (Just "Layout")
        & Select.setAttributes [class_ "fill-inner"]
        & Select.setSelected (Just $ st ^. cloneLens optic)
        & Select.setOnChange (\x -> pureUpdate 0 (& cloneLens optic .~ x))
    )
    ( SelectItem.selectItem
        (SelectItem.config item)
        [text $ userMsg item]
    )
    $ fmap
      ( \t ->
          SelectItem.selectItem
            (SelectItem.config t)
            [text $ userMsg t]
      )
      items
  where
    item :| items = enumerateNE @AssetsAndPaymentsLayout
    optic :: ALens' Model AssetsAndPaymentsLayout
    optic = #modelState . #stDoc . #stDocAssetsAndPaymentsLayout
    userMsg :: AssetsAndPaymentsLayout -> Text
    userMsg = \case
      AssetsBeforePayments -> "Assets before payments"
      PaymentsBeforeAssets -> "Payments before assets"
