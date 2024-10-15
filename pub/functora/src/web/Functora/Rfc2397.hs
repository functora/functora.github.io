module Functora.Rfc2397
  ( encode,
    decode,
  )
where

import qualified Data.ByteString.Base64 as B64
import Functora.Prelude
import qualified Network.URI.Encode as UE
import qualified Prelude

data ENC = BASE64 | URL
  deriving stock (Eq)

encode :: String -> ByteString -> String
encode mime bs =
  "data:"
    <> mime
    <> ";base64,"
    <> either impureThrow id (decodeUtf8Strict $ B64.encode bs)

decode :: String -> Maybe (String, ByteString)
decode url =
  let (scheme, rest) = break (== ':') url
   in case rest of
        ':' : contents | scheme == "data" -> decodeContents contents
        _ -> Nothing

decodeContents :: String -> Maybe (String, ByteString)
decodeContents xs =
  let (prefix, restdata) = break (== ',') xs
   in case restdata of
        ',' : thedata -> decodePrefix prefix thedata
        _ -> Nothing

decodePrefix :: String -> String -> Maybe (String, ByteString)
decodePrefix prefix thedata =
  let fragments = breakList (== ';') prefix
      enc = case reverse fragments of
        ("base64" : _) -> BASE64
        _ -> URL
      mediapart
        | enc == BASE64 = Prelude.init fragments
        | otherwise = fragments
   in case mediapart of
        (xs : _) ->
          case break (== '/') xs of
            (_, []) -> decodeData ("text/plain" : mediapart) enc thedata
            _ -> decodeData mediapart enc thedata
        _ -> decodeData ["text/plain", "charset=US-ASCII"] enc thedata

decodeData :: [String] -> ENC -> String -> Maybe (String, ByteString)
decodeData mediatype enc thedata =
  Just
    ( unparse mediatype,
      case enc of
        URL -> encodeUtf8 $ UE.decode thedata
        BASE64 -> either Prelude.error id . B64.decode $ encodeUtf8 thedata
    )

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
