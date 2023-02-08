{-# LANGUAGE BangPatterns #-}

-- | Private helper functions. Note that all contents of this module are excluded
-- from the versioning scheme.
module OpenAI.Client.Internal.Helpers where

import Network.HTTP.Types.Status
import Servant.Client
import Servant.API
import Network.HTTP.Client (Manager)
import qualified Data.Text as T


-- | Your OpenAI API key. Can be obtained from the OpenAI dashboard. Format: @sk-<redacted>@
type ApiKey = T.Text

-- | Holds a 'Manager' and your API key.
data OpenAIClient = OpenAIClient
  { scBasicAuthData :: BasicAuthData,
    scManager :: Manager,
    scMaxRetries :: Int
  }


openaiBaseUrl :: BaseUrl
openaiBaseUrl = BaseUrl Https "api.openai.com" 443 ""

runRequest :: Int -> Int -> IO (Either ClientError a) -> IO (Either ClientError a)
runRequest maxRetries !retryCount makeRequest =
  do
    res <- makeRequest
    case res of
      Right ok -> pure (Right ok)
      Left err@(ConnectionError _) -> maybeRetry err
      Left err@(FailureResponse _ resp)
        | responseStatusCode resp == conflict409 -> maybeRetry err
        | statusCode (responseStatusCode resp) >= 500 -> maybeRetry err
        | otherwise -> pure (Left err)
      Left err -> pure (Left err)
  where
    maybeRetry err =
      if retryCount + 1 >= maxRetries
        then pure (Left err)
        else runRequest maxRetries (retryCount + 1) makeRequest
