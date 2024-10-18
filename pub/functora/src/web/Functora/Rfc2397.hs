module Functora.Rfc2397
  ( Rfc2397 (..),
    Rfc2397Encoding (..),
    encodeRfc2397,
    decodeRfc2397,
  )
where

import qualified Data.ByteString.Base64.Lazy as B64L
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC8
import Functora.Prelude
import qualified Network.URI as URI
import qualified Network.URI.Encode as UE

data Rfc2397 = Rfc2397
  { rfc2397Mime :: BL.ByteString,
    rfc2397Bytes :: BL.ByteString,
    rfc2397Encoding :: Rfc2397Encoding
  }
  deriving stock (Eq, Ord, Show, Data, Generic)

instance Binary Rfc2397

data Rfc2397Encoding
  = Rfc2397EncodingB64
  | Rfc2397EncodingUrl
  deriving stock (Eq, Ord, Show, Read, Data, Generic, Bounded, Enum)

instance Binary Rfc2397Encoding

encodeRfc2397 :: Rfc2397 -> BL.ByteString
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
              B64L.encode bs
            Rfc2397EncodingUrl ->
              either
                impureThrow
                ( BLC8.pack . UE.encodeWith URI.isAllowedInURI
                )
                $ decodeUtf8Strict bs
         )

decodeRfc2397 :: BL.ByteString -> Maybe Rfc2397
decodeRfc2397 raw =
  case BLC8.break (== ':') raw of
    ("data", mimeAndBytes)
      | not (null mimeAndBytes) ->
          splitMimeAndBytes $ drop 1 mimeAndBytes
    _ -> Nothing

splitMimeAndBytes :: BL.ByteString -> Maybe Rfc2397
splitMimeAndBytes mimeAndBytes =
  case BLC8.break (== ',') mimeAndBytes of
    (mime, bs) | not (null bs) -> decodeMime mime $ drop 1 bs
    _ -> Nothing

decodeMime :: BL.ByteString -> BL.ByteString -> Maybe Rfc2397
decodeMime encMime encBs =
  case mime of
    [] -> decodeBytes ["text/plain", "charset=US-ASCII"] enc encBs
    (typ : _) ->
      case BLC8.break (== '/') typ of
        (_, sub) | null sub -> decodeBytes ("text/plain" : mime) enc encBs
        _ -> decodeBytes mime enc encBs
  where
    (enc, mime) =
      case reverse $ breakList (== ';') encMime of
        ("base64" : xs) -> (Rfc2397EncodingB64, reverse xs)
        xs -> (Rfc2397EncodingUrl, reverse xs)

decodeBytes ::
  [BL.ByteString] -> Rfc2397Encoding -> BL.ByteString -> Maybe Rfc2397
decodeBytes mime enc encBs = do
  bs <-
    case enc of
      Rfc2397EncodingUrl ->
        pure
          . encodeUtf8
          . UE.decode
          =<< either (const Nothing) Just (decodeUtf8Strict encBs)
      Rfc2397EncodingB64 ->
        either (const Nothing) Just $ B64L.decode encBs
  pure
    Rfc2397
      { rfc2397Mime = intercalate ";" mime,
        rfc2397Bytes = bs,
        rfc2397Encoding = enc
      }

breakList :: (Char -> Bool) -> BL.ByteString -> [BL.ByteString]
breakList f xs =
  case BLC8.break f xs of
    (lhs, rhs) | null rhs -> [lhs]
    (lhs, rhs) -> lhs : breakList f (drop 1 rhs)
