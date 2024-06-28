{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module Functora.Web
  ( Opts (..),
    defOpts,
    webFetch,
  )
where

import qualified Data.ByteString.Lazy as BL
import Functora.Prelude
import Functora.WebOrphan ()
import qualified Network.URI as NetURI
import qualified Text.URI as URI

#ifndef __GHCJS__
#ifndef wasi_HOST_OS
import Functora.WebOrphan ()
import qualified Network.HTTP.Client as Web
import qualified Network.HTTP.Client.TLS as Tls
import qualified Network.HTTP.Types as Web
#endif
#endif

#ifdef __GHCJS__
import qualified Data.JSString as JSString
import qualified JavaScript.Web.XMLHttpRequest as Xhr
#endif

#ifdef wasi_HOST_OS
import qualified Servant.Client.JSaddle as Srv
import qualified Servant.API as Srv
import qualified GHCJS.DOM.Types as JSDOM
import qualified Network.HTTP.Media as M
#endif

#ifndef wasi_HOST_OS
newtype Opts = Opts
  { optsQueryString :: [(ByteString, Maybe ByteString)]
  }
  deriving stock (Eq, Generic)

defOpts :: Opts
defOpts =
  Opts
    { optsQueryString = mempty
    }
#else
data Opts = Opts
  { optsQueryString :: [(ByteString, Maybe ByteString)],
    optsJSContextRef :: JSDOM.JSContextRef
  }
  deriving stock (Eq, Generic)

defOpts :: JSDOM.JSContextRef -> Opts
defOpts ctx =
  Opts
    { optsQueryString = mempty,
      optsJSContextRef = ctx
    }
#endif

webFetch ::
  ( MonadIO m,
    MonadThrow m
  ) =>
  URI ->
  Opts ->
  m BL.ByteString
webFetch prevUri opts = do

--
-- TODO : do not ignore qs, ua and nonce in wasm version!!!
--
#ifndef wasi_HOST_OS
  let qs = optsQueryString opts
  nonce <- getCurrentPicos
  let prevQuery = URI.uriQuery prevUri
  optsQuery <- mapM (uncurry newQueryParam) $ (inspect nonce, Nothing) : qs
  let nextQuery = prevQuery <> optsQuery
  let nextUri =
        NetURI.unEscapeString
          $ URI.renderStr prevUri {URI.uriQuery = nextQuery}
#endif

#ifndef __GHCJS__
#ifndef wasi_HOST_OS
  webRaw <- Web.parseRequest nextUri
  let ua :: ByteString =
        "Mozilla/5.0 (Windows NT 10.0; rv:102.0) Gecko/20100101 Firefox/102.0"
  let webReq =
        webRaw
          { Web.requestHeaders =
              Web.requestHeaders webRaw <> [("User-Agent", ua)]
          }
  webRes <- liftIO $ Web.httpLbs webReq =<< Web.newManager Tls.tlsManagerSettings
  let webResBody = Web.responseBody webRes
  let webResCode = Web.responseStatus webRes
  if Web.statusIsSuccessful webResCode
    then pure webResBody
    else
      throwString $
        "Bad HTTP status="
          <> inspect @Text webResCode
          <> " of req="
          <> inspect webReq
          <> " with res="
          <> inspect webRes
          <> " with body="
          <> inspect webResBody
#endif
#endif

#ifdef __GHCJS__
  let webReq =
        Xhr.Request
          { Xhr.reqMethod = Xhr.GET,
            Xhr.reqURI = JSString.pack nextUri,
            Xhr.reqLogin = Nothing,
            Xhr.reqHeaders = mempty,
            Xhr.reqWithCredentials = False,
            Xhr.reqData = Xhr.NoData
          }
  webRes <- liftIO $ Xhr.xhrByteString webReq
  let webResBody = Xhr.contents webRes
  let webResCode = Xhr.status webRes
  if webResCode >= 200 && webResCode < 300
    then pure . BL.fromStrict $ fromMaybe mempty webResBody
    else
      throwString $
        "Bad HTTP status="
          <> inspect @Text webResCode
          <> " of req="
          <> inspect nextUri
          <> " with res="
          <> inspect webResBody
#endif

#ifdef wasi_HOST_OS
  let webCtx = optsJSContextRef opts
  let webClient =
         Srv.client $ Proxy @(Srv.Get '[Everything] BL.ByteString)
  webUri <- Srv.parseBaseUrl . NetURI.unEscapeString
              $ URI.renderStr prevUri {URI.uriQuery = mempty}
  let webClientEnv = Srv.mkClientEnv webUri
  webResBody <- JSDOM.runJSM (Srv.runClientM webClient webClientEnv) webCtx
  either throw pure webResBody
#endif

#ifndef wasi_HOST_OS
newQueryParam ::
  (MonadThrow m) => ByteString -> Maybe ByteString -> m URI.QueryParam
newQueryParam keyRaw valRaw = do
  keyTxt <-
    either throw pure
      . tryFrom @(UTF_8 ByteString) @Text
      $ Tagged @"UTF-8" keyRaw
  key <- URI.mkQueryKey keyTxt
  valTxt <-
    traverse
      ( either throw pure
          . tryFrom @(UTF_8 ByteString) @Text
          . Tagged @"UTF-8"
      )
      valRaw
  val <- traverse URI.mkQueryValue valTxt
  pure $ maybe (URI.QueryFlag key) (URI.QueryParam key) val
#endif

#ifdef wasi_HOST_OS
data Everything

instance Srv.Accept Everything where
  contentType _ = "*" M.// "*"

instance Srv.MimeUnrender Everything BL.ByteString where
  mimeUnrender _ = pure
#endif
