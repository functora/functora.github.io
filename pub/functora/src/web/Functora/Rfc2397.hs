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
import qualified Prelude

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
decodeRfc2397 xs =
  case break (== ':') xs of
    ("data", rhs) | not (null rhs) -> decodeContents $ drop 1 rhs
    _ -> Nothing

decodeContents :: Unicode -> Maybe Rfc2397
decodeContents xs =
  case break (== ',') xs of
    (lhs, rhs) | not (null rhs) -> decodePrefix lhs $ drop 1 rhs
    _ -> Nothing

decodePrefix :: Unicode -> Unicode -> Maybe Rfc2397
decodePrefix prefix thedata =
  case mediapart of
    [] -> decodeData ["text/plain", "charset=US-ASCII"] enc thedata
    (xs : _) ->
      case break (== '/') xs of
        (_, []) -> decodeData ("text/plain" : mediapart) enc thedata
        _ -> decodeData mediapart enc thedata
  where
    fragments = breakList (== ';') prefix
    enc = case reverse fragments of
      ("base64" : _) -> Rfc2397EncodingB64
      _ -> Rfc2397EncodingUrl
    mediapart
      | enc == Rfc2397EncodingB64 = Prelude.init fragments
      | otherwise = fragments

decodeData :: [Unicode] -> Rfc2397Encoding -> Unicode -> Maybe Rfc2397
decodeData mediatype enc thedata = do
  bs <-
    case enc of
      Rfc2397EncodingUrl ->
        pure . encodeUtf8 . UE.decode $ from @Unicode @String thedata
      Rfc2397EncodingB64 ->
        either (const Nothing) Just . B64.decode $ encodeUtf8 thedata
  pure
    Rfc2397
      { rfc2397Mime = intercalate ";" mediatype,
        rfc2397Bytes = bs,
        rfc2397Encoding = enc
      }

breakList :: (Char -> Bool) -> Unicode -> [Unicode]
breakList f xs =
  case break f xs of
    (lhs, rhs) | null rhs -> [lhs]
    (lhs, rhs) -> lhs : breakList f (drop 1 rhs)
