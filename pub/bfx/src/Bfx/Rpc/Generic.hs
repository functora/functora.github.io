{-# OPTIONS_HADDOCK show-extensions #-}

module Bfx.Rpc.Generic
  ( pub,
    prv,
  )
where

import Bfx.Class.ToPathPieces
import qualified Bfx.Data.Web as Web
import Bfx.Import
import qualified Crypto.Hash as Crypto (Digest)
import qualified Crypto.Hash.Algorithms as Crypto (SHA384)
import qualified Crypto.MAC.HMAC as Crypto (hmac, hmacGetDigest)
import qualified Data.Aeson as A
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Network.HTTP.Client as Web
import qualified Network.HTTP.Client.TLS as Tls
import qualified Network.HTTP.Types as Web

pub ::
  forall method req res m.
  ( MonadIO m,
    MonadThrow m,
    SingI method,
    ToBaseUrl method,
    ToPathPieces method req,
    ToRequestMethod method,
    FromRpc method res
  ) =>
  [SomeQueryParam] ->
  req ->
  m res
pub qs req = do
  manager <-
    liftIO $ Web.newManager Tls.tlsManagerSettings
  webReq0 <-
    Web.parseRequest
      . T.unpack
      . T.intercalate "/"
      $ coerce (toBaseUrl @method)
      : toPathPieces @method req
  let webReq1 =
        Web.setQueryString
          (unQueryParam <$> qs)
          $ webReq0
            { Web.method =
                encodeUtf8
                  . inspect @Text
                  $ toRequestMethod @method
            }
  webRes <-
    liftIO $ Web.httpLbs webReq1 manager
  let rawRes =
        RawResponse $ Web.responseBody webRes
  if Web.responseStatus webRes == Web.ok200
    then
      either (throw . parserFailure @method webReq1 webRes rawRes) pure
        $ fromRpc @method rawRes
    else
      throw
        $ ErrorWebPub webReq1 webRes

prv ::
  forall method req res m.
  ( MonadThrow m,
    MonadUnliftIO m,
    SingI method,
    ToBaseUrl method,
    ToPathPieces method req,
    ToRequestMethod method,
    ToJSON req,
    FromRpc method res
  ) =>
  Env ->
  req ->
  m res
prv env req = do
  manager <-
    liftIO $ Web.newManager Tls.tlsManagerSettings
  let apiPath =
        T.intercalate "/" $ toPathPieces @method req
  let reqBody = A.encode req
  webReq0 <-
    Web.parseRequest
      . T.unpack
      $ coerce (toBaseUrl @method)
      <> "/"
      <> apiPath
  withNonce (envNonceGen env) $ \nonce' -> do
    let nonce = encodeUtf8 (inspect nonce' :: Text)
    let webReq1 =
          webReq0
            { Web.method = encodeUtf8 . inspect @Text $ toRequestMethod @method,
              Web.requestBody = Web.RequestBodyLBS reqBody,
              Web.requestHeaders =
                [ ( "Content-Type",
                    "application/json"
                  ),
                  ( "bfx-nonce",
                    nonce
                  ),
                  ( "bfx-apikey",
                    encodeUtf8 . unApiKey $ envApiKey env
                  ),
                  ( "bfx-signature",
                    B16.encode
                      . BS.pack
                      . BA.unpack
                      $ sign (envPrvKey env) apiPath nonce reqBody
                  )
                ]
            }
    webRes <-
      liftIO $ Web.httpLbs webReq1 manager
    let rawRes =
          RawResponse $ Web.responseBody webRes
    if Web.responseStatus webRes == Web.ok200
      then
        either (throw . parserFailure @method webReq1 webRes rawRes) pure
          $ fromRpc @method rawRes
      else
        throw
          $ ErrorWebPrv reqBody webReq1 webRes

sign ::
  PrvKey ->
  Text ->
  BS.ByteString ->
  ByteString ->
  Crypto.Digest Crypto.SHA384
sign prvKey apiPath nonce reqBody =
  Crypto.hmacGetDigest
    . Crypto.hmac (encodeUtf8 $ unPrvKey prvKey :: BS.ByteString)
    $ "/api/"
    <> encodeUtf8 apiPath
    <> nonce
    <> BL.toStrict reqBody

--
-- TODO : add ParserFailure type instead of Text?
--
parserFailure ::
  forall (method :: Method).
  ( SingI method
  ) =>
  Web.Request ->
  Web.Response ByteString ->
  Web.RawResponse ->
  Text ->
  Error
parserFailure webReq webRes res err =
  ErrorParser webReq webRes
    $ inspect (fromSing (sing :: Sing method))
    <> " failed because "
    <> err
    <> " in "
    <> inspect res
