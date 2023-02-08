module OpenAI.Client.Internal.SSE where

import Network.HTTP.Client 
import OpenAI.Resources
import Data.Aeson
import GHC.Generics
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import           Data.Attoparsec.ByteString.Char8
import           Control.Applicative 
import           Control.Monad (when)
import OpenAI.Client.Internal.Helpers
import OpenAI.Internal.Aeson
import Servant.Client.Core.BaseUrl
import Servant.API

event :: Parser ServerEvent
event = (sevent <|> comment <|> retry) <* eol

sevent :: Parser ServerEvent
sevent = ServerEvent
  <$> optional (string "event" *> char ':' *> chars <* eol)
  <*> optional (string "id"    *> char ':' *> chars <* eol)
  <*> fmap BSL.fromChunks (many     (string "data"  *> char ':' *> chars <* eol))

comment :: Parser ServerEvent
comment = CommentEvent <$> (char ':' *> chars <* eol)

retry :: Parser ServerEvent
retry = RetryEvent <$> (string "retry:" *> decimal <* eol)

chars :: Parser BS.ByteString
chars = takeTill (== '\n')

eol :: Parser Char
eol = char '\n'

data ServerEvent
    = ServerEvent {
        seEventName :: Maybe BS.ByteString,
        seEventId   :: Maybe BS.ByteString,
        seEventData :: BSL.ByteString
        }
    | CommentEvent {
        seEventComment :: BS.ByteString
        }
    | RetryEvent {
        seEventRetry :: Int
        }
    | CloseEvent

-- Make a type called Event for the following json
-- {"id": "cmpl-XXXXXXXXXXXXXXXXXXXXXXX", "object": "text_completion", "created": 1671700494, "choices": [{"text": " way", "index": 0, "logprobs": null, "finish_reason": null}], "model": "text-davinci-003"}
data TextCompletionEvent = TextCompletionEvent { 
  tceId :: String,
  tceObject :: String,
  tceCreated :: Int,
  tceChoices :: [TextCompletionChoice],
  tceModel :: String
} deriving stock (Show, Eq, Generic)

$(deriveJSON (jsonOpts 3) ''TextCompletionEvent)

decodeEvent :: (Either String TextCompletionEvent -> IO ()) -> Either String ServerEvent -> IO ()
decodeEvent f = \case 
  Left e -> f $ Left e
  Right (ServerEvent _ _ d) -> f $ eitherDecode d
  Right _ -> pure ()

-- using withServerEvents, make a withEvents function that takes a Manager, a String, 
-- and a function that takes an Either String Event and returns an IO ()
withEvents :: OpenAIClient -> TextCompletionCreate -> (Either String TextCompletionEvent -> IO ()) -> IO ()
withEvents c tc f = withServerEvents 
  (scManager c) 
  (scBasicAuthData c) 
  (showBaseUrl openaiBaseUrl <> "/v1/completions") 
  (encode tc)
  (decodeEvent f)

withServerEvents :: Manager -> BasicAuthData -> String -> BSL.ByteString -> (Either String ServerEvent -> IO ()) -> IO ()
withServerEvents m (BasicAuthData _ key) url postBody f = do 
    -- make a request from the url
  initialRequest <- parseRequest url
  let req = initialRequest 
        { method = "POST"
        , requestBody = RequestBodyLBS postBody 
        , requestHeaders = [ ("Content-Type", "application/json")
                           , ("Accept", "text/event-stream")
                           , ("Authorization", "Bearer " <> key)
                           ]
        }

  let 
    parseLoop :: IO BS.ByteString -> BS.ByteString -> IO ()
    parseLoop source partial = do
        -- get the body of the response
      body <- source
      putStrLn $ "body: " <> show body
      -- list the last 8 bytes of the body and split into two parts
      let (firstPart, lastPart) = BS.splitAt (BS.length body - 14) body

      let 
        bytesToDecode 
          = if (lastPart == "data: [DONE]\n\n") then
              if (firstPart == "") then 
                ""
              else 
                firstPart
            else 
              body
 
      when (not (BS.null bytesToDecode) || not (BS.null partial)) $ do 
        parseWith source event (partial <> bytesToDecode) >>= \case 
          Done i r -> do 
            f $ Right r
            parseLoop source i
          Partial _ -> f $ Left "Unexpected end of input"
          Fail _ _ e -> f $ Left e
        
  withResponse req m $ \resp -> do 
    let bodyReader = responseBody resp
    parseLoop bodyReader ""