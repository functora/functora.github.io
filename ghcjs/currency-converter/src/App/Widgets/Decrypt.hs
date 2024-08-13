module App.Widgets.Decrypt
  ( decrypt,
  )
where

import qualified App.Misc as Misc
import App.Types
import Data.Functor.Barbie
import qualified Functora.Aes as Aes
import Functora.Cfg
import qualified Functora.Miso.Jsm as Jsm
import Functora.Miso.Prelude
import qualified Functora.Miso.Widgets.Field as Field
import qualified Functora.Miso.Widgets.Grid as Grid
import qualified Material.Button as Button
import qualified System.Random as Random

decrypt :: Model -> [View Action]
decrypt st =
  [ Grid.bigCell
      . div_ mempty
      $ Field.dynamicFieldViewer
        pushUpdate
        (st ^. #modelState . #stPre),
    Grid.mediumCell
      $ Field.passwordField
        Field.Args
          { Field.argsModel = st,
            Field.argsOptic = #modelState . #stIkm,
            Field.argsAction = pushUpdate
          }
        ( Field.defOpts @Model @Action
            & #optsOnKeyDownAction
            .~ onKeyDownAction st
        ),
    Grid.mediumCell
      $ Button.raised
        ( Button.config
            & Button.setOnClick (pushUpdate $ decryptDoc st)
            & Button.setIcon (Just "login")
            & Button.setAttributes [class_ "fill"]
        )
        "Open"
  ]

decryptDoc :: Model -> JSM (Model -> Model)
decryptDoc Model {modelState = St {stCpt = Nothing}} = do
  Jsm.popupText @MisoString "Nothing to decrypt!"
  pure id
decryptDoc Model {modelState = St {stCpt = Just {}}} = do
  rnd0 <- liftIO Random.newStdGen
  pure $ \case
    st@Model {modelState = St {stCpt = Nothing}} -> st
    st@Model {modelState = St {stCpt = Just cpt}} ->
      let ikm = st ^. #modelState . #stIkm . #fieldOutput
          aes =
            Aes.drvSomeAesKey @Aes.Word256
              $ (st ^. #modelState . #stKm)
              & #kmIkm
              .~ Ikm (encodeUtf8 ikm)
          eDoc = do
            bDoc <-
              maybe (Left "Incorrect password!") Right
                $ Aes.unHmacDecrypt @ByteString aes cpt
            first thd3
              $ decodeBinary bDoc
       in case eDoc of
            Left {} ->
              -- Jsm.popupText e
              st
            Right iDoc ->
              fst . flip runState rnd0 $ do
                uDoc <-
                  btraverse
                    ( \(Identity val) -> do
                        rnd1 <- get
                        let (uid, rnd2) = randomByteStringPure 32 rnd1
                        put rnd2
                        pure $ Unique (Uid uid) val
                    )
                    iDoc
                pure
                  $ st
                  & #modelState
                  . #stCpt
                  .~ Nothing
                  & #modelState
                  . #stIkm
                  . #fieldInput
                  . #uniqueValue
                  .~ ikm
                  & #modelState
                  . #stIkm
                  . #fieldOutput
                  .~ ikm
                  & #modelState
                  . #stDoc
                  .~ uDoc
                  & #modelState
                  . #stPre
                  .~ (st ^. #modelState . #stPre)
                  & #modelState
                  . #stScreen
                  .~ unQrCode (st ^. #modelState . #stScreen)

onKeyDownAction :: Model -> Uid -> KeyCode -> JSM (Model -> Model)
onKeyDownAction st uid code =
  if code == KeyCode 13
    then decryptDoc st
    else Misc.onKeyDownAction uid code
