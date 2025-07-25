{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}

module App.Types
  ( Model (..),
    Action (..),
    St (..),
    newSt,
    newFieldPair,
    mkUri,
    unUri,
    emitter,
    icon,
    vsn,
    functoraLink,
    sourceLink,
    module X,
  )
where

import qualified Data.ByteString.Base64.URL as B64URL
import qualified Data.ByteString.Lazy as BL
import Data.Functor.Barbie
import qualified Data.Generics as Syb
import qualified Data.Version as Version
import qualified Functora.Aes as Aes
import Functora.Cfg
import Functora.Miso.Prelude
import qualified Functora.Miso.Theme as Theme
import Functora.Miso.Types as X hiding (newFieldPair)
import qualified Functora.Miso.Types as FM
import qualified Functora.Miso.Widgets.Field as Field
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
    modelDonateViewer :: [FieldPair DynamicField Unique]
  }
  deriving stock (Eq, Generic)

data Action
  = Noop
  | SyncInputs
  | InitUpdate (Maybe (St Unique))
  | EvalUpdate (Model -> Model)
  | PushUpdate (Update Model)

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
    stReqMsg :: Maybe Aes.Crypto
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
  qxs <-
    stQuery
      . Syb.everywhere
        ( Syb.mkT
            $ const Blurred
        )
      . Syb.everywhere
        ( Syb.mkT $ \x ->
            if x ^. #fieldType /= FieldTypeImage
              then x :: Field DynamicField Identity
              else
                x
                  & #fieldInput
                  .~ mempty
                  & #fieldOutput
                  .~ DynamicFieldText mempty
        )
      $ uniqueToIdentity
        ( st ^. #modelState
        )
  pure
    $ uri
      { URI.uriQuery = qxs
      }

unUri ::
  ( MonadIO m,
    MonadThrow m
  ) =>
  URI ->
  m (Maybe (St Unique))
unUri uri = do
  kSt <- URI.mkQueryKey "d"
  case qsGet kSt $ URI.uriQuery uri of
    Nothing -> pure Nothing
    Just tSt -> do
      bSt <- either throwString pure . B64URL.decode $ encodeUtf8 tSt
      iSt <- either (throwString . thd3) pure $ decodeBinary bSt
      uSt <-
        identityToUnique
          $ Syb.everywhere (Syb.mkT Field.expandDynamicField) iSt
      pure
        $ Just uSt

stQuery :: (MonadThrow m) => St Identity -> m [URI.QueryParam]
stQuery st = do
  kSt <- URI.mkQueryKey "startapp"
  vSt <- URI.mkQueryValue <=< encode $ encodeBinary st
  pure [URI.QueryParam kSt vSt]
  where
    encode :: (MonadThrow m) => BL.ByteString -> m Text
    encode =
      either throw pure
        . decodeUtf8Strict
        . B64URL.encode
        . from @BL.ByteString @ByteString

baseUri :: Unicode
#ifdef GHCID
baseUri =
  "http://localhost:8080"
#else
baseUri =
  "https://functora.github.io/apps/cryptogram/" <> vsn <> "/index.html"
#endif

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
