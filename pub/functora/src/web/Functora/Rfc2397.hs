module Functora.Rfc2397
  ( Rfc2397 (..),
    Rfc2397Encoding (..),
    encodeRfc2397,
    decodeRfc2397,
  )
where

import qualified Data.ByteString.Base64 as B64
import Functora.Prelude
import qualified Network.URI as URI
import qualified Network.URI.Encode as UE

data Rfc2397 = Rfc2397
  { rfc2397Mime :: Unicode,
    rfc2397Bytes :: ByteString,
    rfc2397Encoding :: Rfc2397Encoding
  }
  deriving stock (Eq, Ord, Show, Data, Generic)

instance Binary Rfc2397

data Rfc2397Encoding
  = Rfc2397EncodingB64
  | Rfc2397EncodingUrl
  deriving stock (Eq, Ord, Show, Read, Data, Generic, Bounded, Enum)

instance Binary Rfc2397Encoding

encodeRfc2397 :: Rfc2397 -> Unicode
encodeRfc2397
  Rfc2397
    { rfc2397Mime = mime,
      rfc2397Bytes = bs,
      rfc2397Encoding = enc
    } =
    "data:"
      <> mime
      <> ( case enc of
            Rfc2397EncodingB64 -> ";base64"
            Rfc2397EncodingUrl -> mempty
         )
      <> ","
      <> ( case enc of
            Rfc2397EncodingB64 ->
              either impureThrow id
                . decodeUtf8Strict
                $ B64.encode bs
            Rfc2397EncodingUrl ->
              either
                impureThrow
                ( from @String @Unicode
                    . UE.encodeWith URI.isAllowedInURI
                )
                $ decodeUtf8Strict bs
         )

decodeRfc2397 :: Unicode -> Maybe Rfc2397
decodeRfc2397 raw =
  case break (== ':') raw of
    ("data", mimeAndBytes)
      | not (null mimeAndBytes) ->
          splitMimeAndBytes $ drop 1 mimeAndBytes
    _ -> Nothing

splitMimeAndBytes :: Unicode -> Maybe Rfc2397
splitMimeAndBytes mimeAndBytes =
  case break (== ',') mimeAndBytes of
    (mime, bs) | not (null bs) -> decodeMime mime $ drop 1 bs
    _ -> Nothing

decodeMime :: Unicode -> Unicode -> Maybe Rfc2397
decodeMime encMime encBs =
  case mime of
    [] -> decodeBytes ["text/plain", "charset=US-ASCII"] enc encBs
    (typ : _) ->
      case break (== '/') typ of
        (_, []) -> decodeBytes ("text/plain" : mime) enc encBs
        _ -> decodeBytes mime enc encBs
  where
    (enc, mime) =
      case reverse $ breakList (== ';') encMime of
        ("base64" : xs) -> (Rfc2397EncodingB64, reverse xs)
        xs -> (Rfc2397EncodingUrl, reverse xs)

decodeBytes :: [Unicode] -> Rfc2397Encoding -> Unicode -> Maybe Rfc2397
decodeBytes mime enc encBs = do
  bs <-
    case enc of
      Rfc2397EncodingUrl ->
        pure . encodeUtf8 . UE.decode $ from @Unicode @String encBs
      Rfc2397EncodingB64 ->
        either (const Nothing) Just . B64.decode $ encodeUtf8 encBs
  pure
    Rfc2397
      { rfc2397Mime = intercalate ";" mime,
        rfc2397Bytes = bs,
        rfc2397Encoding = enc
      }

breakList :: (Char -> Bool) -> Unicode -> [Unicode]
breakList f xs =
  case break f xs of
    (lhs, rhs) | null rhs -> [lhs]
    (lhs, rhs) -> lhs : breakList f (drop 1 rhs)
