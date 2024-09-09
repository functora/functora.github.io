module App.Widgets.Decrypt
  ( decrypt,
  )
where

import App.Types
import qualified App.Widgets.Bolt11 as B11
import Data.Functor.Barbie
import qualified Functora.Aes as Aes
import Functora.Cfg
import qualified Functora.Miso.Css as Css
import qualified Functora.Miso.Jsm as Jsm
import Functora.Miso.Prelude
import qualified Functora.Miso.Widgets.Field as Field
import qualified Functora.Miso.Widgets.Grid as Grid
import qualified Material.Button as Button
import qualified System.Random as Random

decrypt :: Model -> [View Action]
decrypt st =
  [ Grid.bigCell
      $ Field.fieldViewer
        Field.Args
          { Field.argsModel = st,
            Field.argsOptic = #modelState . #stPre,
            Field.argsAction = PushUpdate . Instant
          },
    Grid.mediumCell
      [ Field.passwordField
          Field.Args
            { Field.argsModel = st,
              Field.argsOptic = #modelState . #stIkm,
              Field.argsAction = PushUpdate . Instant
            }
          ( Field.defOpts @Model @Action
              & #optsOnKeyDownAction
              .~ onKeyDownAction
          )
      ],
    Grid.mediumCell
      [ Button.raised
          ( Button.config
              & Button.setOnClick (PushUpdate $ Instant decryptDoc)
              & Button.setIcon (Just "login")
              & Button.setAttributes [Css.fullWidth]
          )
          "Open"
      ]
  ]

decryptDoc :: Model -> JSM Model
decryptDoc st@Model {modelState = St {stCpt = Nothing}} = do
  Jsm.popupText @MisoString "Nothing to decrypt!"
  pure st
decryptDoc st@Model {modelState = St {stCpt = Just cpt}} = do
  let ikm = st ^. #modelState . #stIkm . #fieldOutput
  let aes =
        Aes.drvSomeAesKey @Aes.Word256
          $ (st ^. #modelState . #stKm)
          & #kmIkm
          .~ Ikm (encodeUtf8 ikm)
  let eDoc = do
        bDoc <-
          maybe (Left "Incorrect password!") Right
            $ Aes.unHmacDecrypt @ByteString aes cpt
        bimap
          thd3
          (\doc -> B11.mergeBolt11Viewers (B11.makeBolt11Viewer doc) doc)
          $ decodeBinary bDoc
  case eDoc of
    Left e -> do
      Jsm.popupText e
      pure st
    Right iDoc -> do
      rnd0 <- liftIO Random.newStdGen
      pure . fst . flip runState rnd0 $ do
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

onKeyDownAction :: Uid -> KeyCode -> Model -> JSM Model
onKeyDownAction uid code =
  if code == KeyCode 13
    then decryptDoc
    else Jsm.enterOrEscapeBlur uid code
