{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module BitfinexClient.Class.ToRequestParam
  ( ToRequestParam (..),
    SomeQueryParam (..),
    unQueryParam,
  )
where

import BitfinexClient.Data.Web
import BitfinexClient.Import.External
import qualified Data.ByteString as BS
import qualified Data.Text as T

class ToRequestParam a where
  toTextParam :: a -> Text
  toQueryParam :: a -> Maybe BS.ByteString
  toQueryParam = Just . encodeUtf8 . toTextParam

data SomeQueryParam
  = forall a.
    ( ToRequestParam a
    ) =>
    SomeQueryParam BS.ByteString a

unQueryParam ::
  SomeQueryParam ->
  (BS.ByteString, Maybe BS.ByteString)
unQueryParam (SomeQueryParam name x) =
  (name, toQueryParam x)

data E8

instance HasResolution E8 where
  resolution = const 100000000

instance ToRequestParam Rational where
  toTextParam x =
    T.pack $
      showFixed True (fromRational x :: Fixed E8)

instance ToRequestParam Natural where
  toTextParam =
    T.pack . show

instance ToRequestParam UTCTime where
  toTextParam =
    toTextParam . utcTimeToMicros

instance ToRequestParam Text where
  toTextParam =
    id
