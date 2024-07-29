module App.Widgets.Decrypt
  ( decrypt,
  )
where

import qualified App.Misc as Misc
import App.Types
import qualified App.Widgets.Cell as Cell
import qualified App.Widgets.Field as Field
import Data.Functor.Barbie
import qualified Functora.Aes as Aes
import Functora.Cfg
import Functora.Miso.Prelude
import qualified Material.Button as Button
import qualified System.Random as Random

decrypt :: Model -> [View Action]
decrypt st =
  [ Cell.bigCell
      . div_ mempty
      $ Field.dynamicFieldViewer st (st ^. #modelState . #stPre),
    Cell.mediumCell
      $ Field.passwordField
        st
        ( #modelState
            . #stIkm
        )
        ( Field.defOpts
            & #optsOnKeyDownAction
            .~ onKeyDownAction st
        ),
    Cell.mediumCell
      $ Button.raised
        ( Button.config
            & Button.setOnClick (decryptDoc st)
            & Button.setIcon (Just "login")
            & Button.setAttributes [class_ "fill"]
        )
        "Open"
  ]

decryptDoc :: Model -> Action
decryptDoc st@Model {modelState = St {stCpt = Nothing}} =
  PushUpdate $ do
    Misc.textPopup @MisoString st "Nothing to decrypt!"
    pure $ ChanItem 0 id
decryptDoc Model {modelState = St {stCpt = Just {}}} =
  PushUpdate $ do
    rnd0 <- liftIO Random.newStdGen
    pure $ ChanItem 0 $ \case
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
              Left e ->
                Misc.textPopupPure e st
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
                    & Misc.textPopupClear

onKeyDownAction :: Model -> Uid -> KeyCode -> Action
onKeyDownAction st uid code =
  if code == KeyCode 13
    then decryptDoc st
    else Misc.onKeyDownAction uid code
