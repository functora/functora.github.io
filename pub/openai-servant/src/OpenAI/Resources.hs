{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module OpenAI.Resources
  ( -- * Core Types
    TimeStamp (..),
    OpenAIList (..),

    -- * Text completion
    TextCompletionId (..),
    TextCompletionChoice (..),
    TextCompletion (..),
    TextCompletionCreate (..),
    defaultTextCompletionCreate,

    -- * Embeddings
    EmbeddingCreate (..),
    Embedding (..),

    -- * Fine tuning
    FineTuneId (..),
    FineTuneCreate (..),
    defaultFineTuneCreate,
    FineTune (..),
    FineTuneEvent (..),

    -- * File API
    FileCreate (..),
    FileId (..),
    File (..),
    FileHunk (..),
    ClassificationHunk (..),
    FineTuneHunk (..),
    FileDeleteConfirmation (..),

    -- * Answers API
    AnswerReq (..),
    AnswerResp (..),
  )
where

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import Data.Time
import Data.Time.Clock.POSIX
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import OpenAI.Internal.Aeson
import Servant.API
import Servant.Multipart.API
import qualified Data.HashMap.Strict as HM

-- | A 'UTCTime' wrapper that has unix timestamp JSON representation
newtype TimeStamp = TimeStamp {unTimeStamp :: UTCTime}
  deriving (Show, Eq)

instance A.ToJSON TimeStamp where
  toJSON = A.Number . fromRational . toRational . utcTimeToPOSIXSeconds . unTimeStamp

instance A.FromJSON TimeStamp where
  parseJSON =
    A.withScientific "unix timestamp" $ \sci ->
      pure $ TimeStamp $ posixSecondsToUTCTime (fromRational $ toRational sci)

instance ToHttpApiData TimeStamp where
  toUrlPiece x =
    let unix :: Int
        unix = round . utcTimeToPOSIXSeconds . unTimeStamp $ x
     in T.pack (show unix)

-- | A list wrapper.
newtype OpenAIList a = OpenAIList
  { olData :: [a]
  }
  deriving (Show, Eq, Functor)

instance Semigroup (OpenAIList a) where
  (<>) a b = OpenAIList (olData a <> olData b)

instance Monoid (OpenAIList a) where
  mempty = OpenAIList mempty

instance Applicative OpenAIList where
  pure = OpenAIList . pure
  (<*>) go x = OpenAIList (olData go <*> olData x)

newtype TextCompletionId = TextCompletionId {unTextCompletionId :: T.Text}
  deriving (Show, Eq, ToJSON, FromJSON, ToHttpApiData)

data TextCompletionChoice = TextCompletionChoice
  { tccText :: T.Text,
    tccIndex :: Int,
    tccLogProps :: Maybe Int,
    tccFinishReason :: Maybe T.Text
  }
  deriving (Show, Eq)

data TextCompletion = TextCompletion
  { tcId :: TextCompletionId,
    tcCreated :: TimeStamp,
    tcModel :: T.Text,
    tcChoices :: V.Vector TextCompletionChoice
  }
  deriving (Show, Eq)

data TextCompletionCreate = TextCompletionCreate
  { tccrModel :: T.Text,
    tccrPrompt :: T.Text, -- TODO: support lists of strings
    tccrSuffix :: Maybe T.Text,
    tccrMaxTokens :: Maybe Int,
    tccrTemperature :: Maybe Double,
    tccrTopP :: Maybe Double,
    tccrN :: Maybe Int,
    tccrStream :: Maybe Bool,
    tccrLogprobs :: Maybe Int,
    tccrEcho :: Maybe Bool,
    tccrStop :: Maybe (V.Vector T.Text),
    tccrPresencePenalty :: Maybe Double,
    tccrFrequencyPenalty :: Maybe Double,
    tccrBestOf :: Maybe Int,
    tccrLogitBias :: Maybe (HM.HashMap T.Text Double),
    tccrUser  :: Maybe T.Text
  }
  deriving (Show, Eq)

-- | Applies API defaults, only passing a prompt.
defaultTextCompletionCreate :: T.Text -> TextCompletionCreate
defaultTextCompletionCreate prompt =
  TextCompletionCreate
    { tccrModel = "text-embedding-ada-002",
      tccrPrompt = prompt,
      tccrSuffix = Nothing,
      tccrMaxTokens = Nothing,
      tccrTemperature = Nothing,
      tccrTopP = Nothing,
      tccrN = Nothing,
      tccrStream = Nothing,
      tccrLogprobs = Nothing,
      tccrEcho = Nothing,
      tccrStop = Nothing,
      tccrPresencePenalty = Nothing,
      tccrFrequencyPenalty = Nothing,
      tccrBestOf = Nothing,
      tccrLogitBias = Nothing,
      tccrUser = Nothing
    }

data EmbeddingCreate = EmbeddingCreate
  { ecInput :: [T.Text]
  , ecModel :: T.Text
  , ecUser  :: Maybe T.Text
  }
  deriving (Show, Eq)

data Embedding = Embedding
  {eEmbedding :: VS.Vector Double, eIndex :: Int}
  deriving (Show, Eq)

newtype FineTuneId = FineTuneId {unFineTuneId :: T.Text}
  deriving (Show, Eq, ToJSON, FromJSON, ToHttpApiData)

data FineTuneCreate = FineTuneCreate
  { ftcTrainingFile :: FileId,
    ftcValidationFile :: Maybe FileId,
    ftcModel :: Maybe T.Text,
    ftcBatchSize :: Maybe Int,
    ftcNEpochs :: Maybe T.Text,
    ftcLearningRateMultiplier :: Maybe Double,
    ftcPromptLossWeight :: Maybe Double,
    ftcComputeClassificationMetrics :: Maybe Bool,
    ftcClassificationNClasses :: Maybe Int,
    ftcClassificationPositiveClass :: Maybe T.Text
  }
  deriving (Show, Eq)

defaultFineTuneCreate :: FileId -> FineTuneCreate
defaultFineTuneCreate file =
  FineTuneCreate
    { ftcTrainingFile = file,
      ftcValidationFile = Nothing,
      ftcModel = Nothing,
      ftcBatchSize = Nothing,
      ftcNEpochs = Nothing,
      ftcLearningRateMultiplier = Nothing,
      ftcPromptLossWeight = Nothing,
      ftcComputeClassificationMetrics = Nothing,
      ftcClassificationNClasses = Nothing,
      ftcClassificationPositiveClass = Nothing
    }

data FineTuneEvent = FineTuneEvent
  { fteCreatedAt :: Int,
    fteLevel :: T.Text,
    fteMessage :: T.Text
  }
  deriving (Show, Eq)

data FineTune = FineTune
  { ftId :: FineTuneId,
    ftModel :: T.Text,
    ftCreatedAt :: Int,
    ftEvents :: V.Vector FineTuneEvent,
    ftTunedModel :: Maybe T.Text,
    ftStatus :: T.Text
  }
  deriving (Show, Eq)

data ClassificationHunk = ClassificationHunk
  { chText :: T.Text,
    chLabel :: T.Text
  }
  deriving (Show, Eq)

data FineTuneHunk = FineTuneHunk
  { fthPrompt :: T.Text,
    fthCompletion :: T.Text
  }
  deriving (Show, Eq)

data FileHunk
  = FhClassifications ClassificationHunk
  | FhFineTune FineTuneHunk
  deriving (Show, Eq)

data FileCreate = FileCreate
  { fcPurpose :: T.Text,
    fcDocuments :: [FileHunk]
  }
  deriving (Show, Eq)

newtype FileId = FileId {unFileId :: T.Text}
  deriving (Show, Eq, ToJSON, FromJSON, ToHttpApiData)

data File = File
  { fId :: FileId,
    fCreatedAt :: TimeStamp,
    fStatus :: T.Text,
    fPurpose :: T.Text
  }
  deriving (Show, Eq)

data FileDeleteConfirmation = FileDeleteConfirmation
  { fdcId :: FileId
  }
  deriving (Show, Eq)

data AnswerReq = AnswerReq
  { arFile :: Maybe FileId,
    arDocuments :: Maybe (V.Vector T.Text),
    arQuestion :: T.Text,
    arModel :: T.Text,
    arExamplesContext :: T.Text,
    arExamples :: [[T.Text]],
    arReturnMetadata :: Bool
  }
  deriving (Show, Eq)

data AnswerResp = AnswerResp
  { arsAnswers :: [T.Text]
  }
  deriving (Show, Eq)

$(deriveJSON (jsonOpts 2) ''OpenAIList)
$(deriveJSON (jsonOpts 3) ''TextCompletionChoice)
$(deriveJSON (jsonOpts 2) ''TextCompletion)
$(deriveJSON (jsonOpts 4) ''TextCompletionCreate)
$(deriveJSON (jsonOpts 1) ''File)
$(deriveJSON (jsonOpts 3) ''FileDeleteConfirmation)
$(deriveJSON (jsonOpts 2) ''AnswerReq)
$(deriveJSON (jsonOpts 3) ''AnswerResp)
$(deriveJSON (jsonOpts 2) ''EmbeddingCreate)
$(deriveJSON (jsonOpts 1) ''Embedding)
$(deriveJSON (jsonOpts 3) ''FineTuneCreate)
$(deriveJSON (jsonOpts 3) ''FineTuneEvent)
$(deriveJSON (jsonOpts 2) ''FineTune)
$(deriveJSON (jsonOpts 2) ''ClassificationHunk)
$(deriveJSON (jsonOpts 3) ''FineTuneHunk)

packDocuments :: [FileHunk] -> BSL.ByteString
packDocuments docs =
  BSL.intercalate "\n" $
    map
      ( \t -> A.encode $
          case t of
            FhClassifications x -> A.toJSON x
            FhFineTune x -> A.toJSON x
      )
      docs

instance ToMultipart Mem FileCreate where
  toMultipart rfc =
    MultipartData
      [ Input "purpose" (fcPurpose rfc)
      ]
      [ FileData "file" "data.jsonl" "application/json" (packDocuments $ fcDocuments rfc)
      ]
