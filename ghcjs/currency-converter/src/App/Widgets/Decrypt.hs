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
import Functora.Prelude as Prelude
import qualified Material.Button as Button
import Miso hiding (at, view)

decrypt :: Model -> [View Action]
decrypt st =
  [ Cell.mediumCell
      $ Field.passwordField
        st
        ( #modelState
            . #stCrypto
            . _Just
            . #stCryptoIkm
        )
        ( Field.defOpts
        --  & #optsOnKeyDownAction
        --  .~ onKeyDownAction
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
decryptDoc st@Model {modelState = St {stCrypto = Nothing}} =
  PushUpdate $ do
    Misc.textPopup @Text st "Nothing to decrypt!"
    pure $ ChanItem 0 id
decryptDoc st@Model {modelState = St {stCrypto = Just cpt}} =
  PushUpdate $ do
    let aes =
          Aes.drvSomeAesKey @Aes.Word256
            $ (cpt ^. #stCryptoKm)
            & #kmIkm
            .~ (cpt ^. #stCryptoIkm . #fieldOutput . to (Ikm . encodeUtf8))
    let eDoc = do
          bDoc <-
            maybe (Left "Incorrect password!") Right
              $ Aes.unHmacDecrypt @ByteString aes (cpt ^. #stCryptoDoc)
          first thd3
            $ decodeBinary bDoc
    case eDoc of
      Left e -> do
        Misc.textPopup st e
        pure $ ChanItem 0 id
      Right iDoc -> do
        uDoc <- btraverse (newUnique . runIdentity) iDoc
        pure
          . ChanItem 0
          $ (& #modelState . #stCrypto .~ Nothing)
          . (& #modelState . #stDoc .~ uDoc)
