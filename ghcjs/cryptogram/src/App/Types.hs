{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}

module App.Types
  ( Model (..),
    Action (..),
    St (..),
    newSt,
    newFieldPair,
    mkUri,
    emitter,
    icon,
    vsn,
    functoraLink,
    sourceLink,
    module X,
  )
where

import Data.Functor.Barbie
import qualified Data.Version as Version
import qualified Functora.Aes as Aes
import Functora.Cfg
import Functora.Miso.Prelude
import qualified Functora.Miso.Theme as Theme
import Functora.Miso.Types as X hiding (newFieldPair)
import qualified Functora.Miso.Types as FM
import qualified Functora.Miso.Widgets.Icon as Icon
import qualified Functora.Prelude as Prelude
import qualified Paths_cryptogram as Paths
import qualified Text.URI as URI

data Model = Model
  { modelSink :: MVar (Action -> IO ()),
    modelMenu :: OpenedOrClosed,
    modelDonate :: OpenedOrClosed,
    modelLoading :: Bool,
    modelState :: St Unique,
    modelChatId :: Int,
    modelDonateViewer :: [FieldPair DynamicField Unique]
  }
  deriving stock (Eq, Generic)

data Action
  = Noop
  | InitUpdate
  | EvalUpdate (Model -> Model)
  | PushUpdate (Update Model)
  | SyncInputs

data St f = St
  { stReq :: StReq,
    stIkm :: Field Unicode f,
    stInp :: Unicode,
    stOut :: Field Unicode f,
    stEnableTheme :: Bool,
    stTheme :: Theme
  }
  deriving stock (Generic)

deriving stock instance (Hkt f) => Eq (St f)

deriving stock instance (Hkt f) => Ord (St f)

deriving stock instance (Hkt f) => Show (St f)

deriving stock instance (Hkt f) => Data (St f)

instance FunctorB St

instance TraversableB St

deriving via GenericType (St Identity) instance Binary (St Identity)

data StReq = StReq
  { stReqKm :: Aes.Km,
    stReqCpt :: Maybe Aes.Crypto
  }
  deriving stock (Eq, Ord, Show, Read, Data, Generic)
  deriving (Binary) via GenericType StReq

newSt :: (MonadIO m) => m (St Unique)
newSt = do
  km <- Aes.randomKm 32
  ikm <- newPasswordField . decodeUtf8 $ km ^. #kmIkm . #unIkm
  out <- newTextField mempty
  pure
    St
      { stReq = StReq km Nothing,
        stIkm = ikm,
        stInp = mempty,
        stOut = out,
        stEnableTheme = True,
        stTheme = Theme.Matcha
      }

newFieldPair ::
  ( MonadIO m
  ) =>
  Unicode ->
  DynamicField ->
  m (FieldPair DynamicField Unique)
newFieldPair key val = do
  res <- FM.newFieldPair key val
  pure
    $ res
    & #fieldPairValue
    . #fieldOpts
    . #fieldOptsAllowCopy
    .~ False

mkUri :: (MonadThrow m) => Model -> m URI
mkUri st = do
  uri <- mkURI $ from @Unicode @Prelude.Text baseUri
  key <- URI.mkQueryKey "startattach"
  val <-
    URI.mkQueryValue
      $ encodeBinaryB64Url
        StReq
          { stReqKm = km,
            stReqCpt = Just cpt
          }
  pure
    $ uri
      { URI.uriQuery = [URI.QueryParam key val]
      }
  where
    km = st ^. #modelState . #stReq . #stReqKm & #kmIkm .~ Ikm mempty
    ikm = encodeUtf8 $ st ^. #modelState . #stIkm . #fieldOutput
    aes = Aes.drvSomeAesKey @Aes.Word256 $ km & #kmIkm .~ Ikm ikm
    msg = encodeBinary $ st ^. #modelState . #stOut . #fieldOutput
    cpt = Aes.encryptHmac aes msg

baseUri :: Unicode
baseUri = "https://t.me/functora_cryptogram_bot"

emitter :: (MonadIO m) => Model -> Update Model -> m ()
emitter st updater = do
  sink <- readMVar $ modelSink st
  liftIO . sink $ PushUpdate updater

icon :: Icon.Icon -> View action
icon = Icon.icon @Icon.Fa

vsn :: Unicode
vsn =
  intercalate "."
    . fmap inspect
    $ Version.versionBranch Paths.version

functoraLink :: URI
functoraLink =
  either impureThrow id
    $ mkURI "https://functora.github.io/"

sourceLink :: URI
sourceLink =
  either impureThrow id
    $ mkURI
      "https://github.com/functora/functora.github.io/tree/master/ghcjs/cryptogram"
