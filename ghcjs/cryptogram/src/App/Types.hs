{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}

module App.Types
  ( Model (..),
    Action (..),
    St (..),
    newSt,
    newFieldPair,
    newFieldPairId,
    mkShortUri,
    unShortUri,
    mkLongUri,
    unLongUri,
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
import Functora.Cfg
import Functora.Miso.Prelude
import qualified Functora.Miso.Theme as Theme
import Functora.Miso.Types as X hiding
  ( newFieldPair,
    newFieldPairId,
  )
import qualified Functora.Miso.Types as FM
import qualified Functora.Miso.Widgets.Field as Field
import qualified Functora.Miso.Widgets.Icon as Icon
import qualified Functora.Prelude as Prelude
import qualified Functora.Web as Web
import qualified Paths_cryptogram as Paths
import qualified Text.URI as URI

data Model = Model
  { modelSink :: MVar (Action -> IO ()),
    modelMenu :: OpenedOrClosed,
    modelDonate :: OpenedOrClosed,
    modelLoading :: Bool,
    modelState :: St Unique,
    modelUriViewer :: [FieldPair DynamicField Unique],
    modelDonateViewer :: [FieldPair DynamicField Unique],
    modelWebOpts :: Web.Opts
  }
  deriving stock (Eq, Generic)

data Action
  = Noop
  | SyncInputs
  | InitUpdate (Maybe (St Unique))
  | EvalUpdate (Model -> Model)
  | PushUpdate (Update Model)

data St f = St
  { stPwd :: Field Unicode f,
    stEncReq :: Field Unicode f,
    stDecReq :: Field Unicode f,
    stDecRes :: Field Unicode f,
    stEnableTheme :: Bool,
    stTheme :: Theme
  }
  deriving stock (Generic)

deriving stock instance (Hkt f) => Eq (St f)

deriving stock instance (Hkt f) => Ord (St f)

deriving stock instance (Hkt f) => Show (St f)

deriving stock instance (Hkt f) => Data (St f)

deriving via (GenericType (St f)) instance (Hkt f) => ToQuery (St f)

deriving via (GenericType (St Identity)) instance FromQuery (St Identity)

instance FunctorB St

instance TraversableB St

deriving via GenericType (St Identity) instance Binary (St Identity)

newSt :: (MonadIO m) => m (St Unique)
newSt = do
  pwd <- newTextField mempty
  encReq <- newTextField mempty
  decReq <- newTextField mempty
  decRes <- newTextField mempty
  pure
    St
      { stPwd = pwd,
        stEncReq = encReq,
        stDecReq = decReq,
        stDecRes = decRes,
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

newFieldPairId ::
  Unicode ->
  DynamicField ->
  FieldPair DynamicField Identity
newFieldPairId key val = do
  FM.newFieldPairId key val
    & #fieldPairValue
    . #fieldOpts
    . #fieldOptsAllowCopy
    .~ False

mkShortUri :: (MonadThrow m) => Model -> m URI
mkShortUri st = do
  uri <- mkURI $ from @Unicode @Prelude.Text baseUri
  let qxs = toQuery . uniqueToIdentity $ modelState st
  pure $ uri {URI.uriQuery = qxs}

unShortUri :: (MonadIO m, MonadThrow m) => URI -> m (St Unique)
unShortUri uri = do
  st <- either throw pure . fromQuery $ URI.uriQuery uri
  identityToUnique st

mkLongUri :: (MonadThrow m) => Model -> m URI
mkLongUri st = do
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
      . uniqueToIdentity
      $ st
      ^. #modelState
  pure
    $ uri
      { URI.uriQuery = qxs
      }

unLongUri ::
  ( MonadIO m,
    MonadThrow m
  ) =>
  URI ->
  m (Maybe (St Unique))
unLongUri uri = do
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
  kSt <- URI.mkQueryKey "d"
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
