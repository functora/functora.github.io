module Functora.Rfc2397
  ( Media (..),
    Encoding (..),
    encode,
    decode,
  )
where

import qualified Data.ByteString.Base64 as B64
import Functora.Prelude
import qualified Network.URI as URI
import qualified Network.URI.Encode as UE
import qualified Prelude

data Media = Media
  { mediaMime :: String,
    mediaBytes :: ByteString,
    mediaEncoding :: Encoding
  }
  deriving stock (Eq, Ord, Show, Data, Generic)

data Encoding = B64Encoding | UrlEncoding
  deriving stock (Eq, Ord, Show, Read, Data, Generic, Bounded, Enum)

encode :: Media -> String
encode Media {mediaMime = mime, mediaBytes = bs, mediaEncoding = enc} =
  "data:"
    <> mime
    <> ( case enc of
          B64Encoding -> ";base64"
          UrlEncoding -> mempty
       )
    <> ","
    <> ( case enc of
          B64Encoding ->
            either impureThrow id
              . decodeUtf8Strict
              $ B64.encode bs
          UrlEncoding ->
            either impureThrow (UE.encodeWith URI.isAllowedInURI)
              $ decodeUtf8Strict bs
       )

decode :: String -> Maybe Media
decode url =
  let (scheme, rest) = break (== ':') url
   in case rest of
        ':' : contents | scheme == "data" -> decodeContents contents
        _ -> Nothing

decodeContents :: String -> Maybe Media
decodeContents xs =
  let (prefix, restdata) = break (== ',') xs
   in case restdata of
        ',' : thedata -> decodePrefix prefix thedata
        _ -> Nothing

decodePrefix :: String -> String -> Maybe Media
decodePrefix prefix thedata =
  let fragments = breakList (== ';') prefix
      enc = case reverse fragments of
        ("base64" : _) -> B64Encoding
        _ -> UrlEncoding
      mediapart
        | enc == B64Encoding = Prelude.init fragments
        | otherwise = fragments
   in case mediapart of
        (xs : _) ->
          case break (== '/') xs of
            (_, []) -> decodeData ("text/plain" : mediapart) enc thedata
            _ -> decodeData mediapart enc thedata
        _ -> decodeData ["text/plain", "charset=US-ASCII"] enc thedata

decodeData :: [String] -> Encoding -> String -> Maybe Media
decodeData mediatype enc thedata = do
  bs <-
    case enc of
      UrlEncoding ->
        pure . encodeUtf8 $ UE.decode thedata
      B64Encoding ->
        either (const Nothing) Just . B64.decode $ encodeUtf8 thedata
  pure
    Media
      { mediaMime = unparse mediatype,
        mediaBytes = bs,
        mediaEncoding = enc
      }

breakList :: (x -> Bool) -> [x] -> [[x]]
breakList p xs =
  let (pre, post) = break p xs
   in case post of
        [] -> [pre]
        _ : ys -> pre : breakList p ys

unparse :: [String] -> String
unparse [] = ""
unparse [xs] = xs
unparse (xs : xss) = xs ++ ';' : unparse xss
