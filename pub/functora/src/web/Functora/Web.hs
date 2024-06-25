{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module Functora.Web
  ( webFetch,
  )
where

import qualified Data.ByteString.Lazy as BL
import Functora.Prelude
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
import qualified Extism.PDK.HTTP as Pdk
#endif

webFetch ::
  ( MonadIO m,
    MonadThrow m
  ) =>
  URI ->
  [(ByteString, Maybe ByteString)] ->
  m BL.ByteString
webFetch prevUri qs = do
  nonce <- getCurrentPicos
  let prevQuery = URI.uriQuery prevUri
  nextQuery <- mapM (uncurry newQueryParam) $ (inspect nonce, Nothing) : qs
  let nextUri =
        NetURI.unEscapeString
          $ URI.renderStr prevUri {URI.uriQuery = prevQuery <> nextQuery}

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
  let ua :: String =
        "Mozilla/5.0 (Windows NT 10.0; rv:102.0) Gecko/20100101 Firefox/102.0"
  let webReq =
        Pdk.Request
          { Pdk.url = nextUri,
            Pdk.headers = [("User-Agent", ua)],
            Pdk.method = "GET"
          }
  webRes <- liftIO $ Pdk.sendRequest webReq (Nothing :: Maybe String)
  webResBody <- liftIO $ Pdk.response webRes
  let webResCode = Pdk.statusCode webRes
  if webResCode >= 200 && webResCode < 300
    then either throwString (pure . BL.fromStrict) webResBody
    else
      throwString $
        "Bad HTTP status="
          <> inspect @Text webResCode
          <> " of req="
          <> inspect nextUri
          <> " with res="
          <> inspect webResBody
#endif

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
