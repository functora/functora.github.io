{-# LANGUAGE TemplateHaskell #-}

module Functora.Web
  ( module X,
    SomeQueryParam (..),
    someQueryParamLabel,
    unSomeQueryParam,
    webCatch,
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

webCatch ::
  ( MonadIO m
  ) =>
  IO (Either Text a) ->
  ExceptT Text m a
webCatch expr =
  ExceptT
    . liftIO
    . catch expr
    $ \(e :: Web.HttpException) ->
      pure
        . Left
        . from @String @Text
        $ displayException e

webFetch ::
  ( MonadIO m
  ) =>
  URI ->
  [SomeQueryParam] ->
  ExceptT Text m BL.ByteString
webFetch url qs = webCatch $ do
  uriReq <- Web.parseRequest $ URI.renderStr url
  let webReq = Web.setQueryString (unSomeQueryParam <$> qs) uriReq
  webRes <- Web.httpLbs webReq =<< Web.newManager Tls.tlsManagerSettings
  let webResBody = Web.responseBody webRes
  let webResCode = Web.responseStatus webRes
  pure $
    if Web.statusIsSuccessful webResCode
      then Right webResBody
      else
        Left $
          "Bad HTTP status="
            <> inspect webResCode
            <> " of req="
            <> inspect webReq
            <> " with res="
            <> inspect webRes
            <> " with body="
            <> inspect webResBody
