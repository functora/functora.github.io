module ApiSpec (apiSpec) where

import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import OpenAI.Client
import System.Environment (getEnv)
import Test.Hspec

makeClient :: IO OpenAIClient
makeClient =
  do
    manager <- newManager tlsManagerSettings
    apiKey <- T.pack <$> getEnv "OPENAI_KEY"
    pure (makeOpenAIClient apiKey manager 2)

forceSuccess :: (MonadFail m, Show a) => m (Either a b) -> m b
forceSuccess req =
  req >>= \res ->
    case res of
      Left err -> fail (show err)
      Right ok -> pure ok

apiSpec :: Spec
apiSpec =
  describe "core api" apiTests

apiTests :: SpecWith ()
apiTests =
  beforeAll makeClient $
    do
      describe "file api" $
        do
          it "allows creating one" $ \cli ->
            do
              pending
              let file =
                    FileCreate
                      { fcPurpose = "fine-tune",
                        fcDocuments = []
                      }
              _ <- forceSuccess $ createFile cli file
              pure ()
      describe "embeddings" $ do
        it "computes embeddings" $ \cli -> do
          res <- forceSuccess $ createEmbedding cli (EmbeddingCreate ["This is nice"] "text-embedding-ada-002" Nothing)
          null (olData res) `shouldBe` False
          let embedding = head (olData res)
          VS.length (eEmbedding embedding) `shouldBe` 1536
      describe "fine tuning" $ do
        it "allows creating fine-tuning" $ \cli -> do
          let file =
                FileCreate
                  { fcPurpose = "fine-tune",
                    fcDocuments =
                      [ FhFineTune $ FineTuneHunk "So sad. Label:" "sad",
                        FhFineTune $ FineTuneHunk "So happy. Label:" "happy"
                      ]
                  }
          createRes <- forceSuccess $ createFile cli file
          let ftc = defaultFineTuneCreate (fId createRes)
          res <- forceSuccess $ createFineTune cli ftc
          ftStatus res `shouldBe` "pending"
      describe "text completion" $
        do
          it "works (smoke test)" $ \cli ->
            do
              completionResults <-
                forceSuccess $
                  completeText cli $
                    (defaultTextCompletionCreate "Why is the house ")
                      { tccrMaxTokens = Just 2
                      }
              V.length (tcChoices completionResults) `shouldBe` 1
              T.length (tccText (V.head (tcChoices completionResults))) `shouldNotBe` 0
