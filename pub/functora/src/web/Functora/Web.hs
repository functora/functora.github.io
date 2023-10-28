{-# LANGUAGE TemplateHaskell #-}

module Functora.Web
  ( module X,
    SomeQueryParam (..),
    someQueryParamLabel,
    unSomeQueryParam,
    webFetch,
  )
where

import qualified Data.ByteString.Lazy as BL
import Functora.Prelude
import Functora.WebOrphan as X ()
import qualified Network.HTTP.Client as Web
import qualified Network.HTTP.Client.TLS as Tls
import qualified Network.HTTP.Types as Web
import qualified Text.URI as URI
import Yesod.Core as X (PathPiece (..))

data SomeQueryParam = forall a.
  ( PathPiece a
  ) =>
  SomeQueryParam
  { _someQueryParamLabel :: ByteString,
    _someQueryParamValue :: a
  }

makeLenses ''SomeQueryParam

unSomeQueryParam ::
  SomeQueryParam ->
  (ByteString, Maybe ByteString)
unSomeQueryParam (SomeQueryParam label value) =
  ( label,
    Just
      . via @(UTF_8 ByteString) @Text @ByteString
      $ toPathPiece value
  )

webFetch ::
  ( MonadIO m,
    MonadThrow m
  ) =>
  URI ->
  [SomeQueryParam] ->
  m BL.ByteString
webFetch uri qs = do
  let prev = URI.uriQuery uri
  next <- forM qs $ uncurry newQueryParam . unSomeQueryParam
  webRaw <- Web.parseRequest $ URI.renderStr uri {URI.uriQuery = prev <> next}
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
  where
    ua :: ByteString
    ua = "Mozilla/5.0 (Windows NT 10.0; rv:102.0) Gecko/20100101 Firefox/102.0"

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
