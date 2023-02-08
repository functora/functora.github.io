{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -cpp -pgmPcpphs -optP--cpp #-}

module OpenAI.Client
  ( -- * Basics
    ApiKey,
    OpenAIClient,
    makeOpenAIClient,
    ClientError (..),

    -- * Helper types
    TimeStamp (..),
    OpenAIList (..),

    -- * Text completion
    TextCompletionId (..),
    TextCompletionChoice (..),
    TextCompletion (..),
    TextCompletionCreate (..),
    defaultTextCompletionCreate,
    completeText,

    -- * Embeddings
    EmbeddingCreate (..),
    Embedding (..),
    createEmbedding,
    createEmbedding2,

    -- * Fine tunes
    FineTuneId (..),
    FineTuneCreate (..),
    defaultFineTuneCreate,
    FineTune (..),
    FineTuneEvent (..),
    createFineTune,
    listFineTunes,
    getFineTune,
    cancelFineTune,
    listFineTuneEvents,

    -- * File API
    FileCreate (..),
    File (..),
    FileId (..),
    FileHunk (..),
    ClassificationHunk (..),
    FineTuneHunk (..),
    FileDeleteConfirmation (..),
    createFile,
    deleteFile,

    -- * Answer API
    getAnswer,
    AnswerReq (..),
    AnswerResp (..),

    -- * SSE
    ServerEvent(..),
    TextCompletionEvent(..),
    withEvents,
    withServerEvents 
  )
where

import qualified Data.ByteString.Lazy as BSL
import Data.Proxy
import qualified Data.Text.Encoding as T
import Network.HTTP.Client (Manager)
import OpenAI.Api
import OpenAI.Client.Internal.Helpers
import OpenAI.Resources
import Servant.API
import Servant.Client
import qualified Servant.Multipart.Client as MP
import OpenAI.Client.Internal.SSE


-- | Construct a 'OpenAIClient'. Note that the passed 'Manager' must support https (e.g. via @http-client-tls@)
makeOpenAIClient ::
  ApiKey ->
  Manager ->
  -- | Number of automatic retries the library should attempt.
  Int ->
  OpenAIClient
makeOpenAIClient k = OpenAIClient (BasicAuthData "" (T.encodeUtf8 k))

api :: Proxy OpenAIApi
api = Proxy


#define EP0(N, R) \
    N##' :: BasicAuthData -> ClientM R;\
    N :: OpenAIClient -> IO (Either ClientError R);\
    N sc = runRequest (scMaxRetries sc) 0 $ runClientM (N##' (scBasicAuthData sc)) (mkClientEnv (scManager sc) openaiBaseUrl)

#define EP(N, ARG, R) \
    N##' :: BasicAuthData -> ARG -> ClientM R;\
    N :: OpenAIClient -> ARG -> IO (Either ClientError R);\
    N sc a = runRequest (scMaxRetries sc) 0 $ runClientM (N##' (scBasicAuthData sc) a) (mkClientEnv (scManager sc) openaiBaseUrl)

#define EP2(N, ARG, ARG2, R) \
    N##' :: BasicAuthData -> ARG -> ARG2 -> ClientM R;\
    N :: OpenAIClient -> ARG -> ARG2 -> IO (Either ClientError R);\
    N sc a b = runRequest (scMaxRetries sc) 0 $ runClientM (N##' (scBasicAuthData sc) a b) (mkClientEnv (scManager sc) openaiBaseUrl)

EP (completeText, TextCompletionCreate, TextCompletion)
EP (createEmbedding, EmbeddingCreate, (OpenAIList Embedding))
EP (createEmbedding2, EmbeddingCreate, (OpenAIList Embedding))

EP (createFineTune, FineTuneCreate, FineTune)
EP0 (listFineTunes, (OpenAIList FineTune))
EP (getFineTune, FineTuneId, FineTune)
EP (cancelFineTune, FineTuneId, FineTune)
EP (listFineTuneEvents, FineTuneId, (OpenAIList FineTuneEvent))

createFile :: OpenAIClient -> FileCreate -> IO (Either ClientError File)
createFile sc rfc =
  do
    bnd <- MP.genBoundary
    createFileInternal sc (bnd, rfc)

EP (createFileInternal, (BSL.ByteString, FileCreate), File)
EP (deleteFile, FileId, FileDeleteConfirmation)

EP (getAnswer, AnswerReq, AnswerResp)

(completeText'
  :<|> createEmbedding'
  :<|> (createFileInternal' :<|> deleteFile')
  :<|> getAnswer'
  :<|> ( createFineTune'
           :<|> listFineTunes'
           :<|> getFineTune'
           :<|> cancelFineTune'
           :<|> listFineTuneEvents'
         ))
  :<|> createEmbedding2' =
    client api
