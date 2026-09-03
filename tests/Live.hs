{-# LANGUAGE OverloadedStrings #-}

module Live (spec) where

import qualified Data.Text as T
import Katip (initLogEnv)
import Network.HTTP.Client.TLS (newTlsManager)
import System.Environment (lookupEnv)
import Test.Hspec

import GenAI.Client

-- | The model this test calls. Google retires model names on its own
-- schedule and returns 404 with a replacement named in the message, so the
-- default is a moving target rather than a fact about this library. Override
-- it with @GEMINI_TEST_MODEL@ instead of editing this file.
defaultTestModel :: T.Text
defaultTestModel = "gemini-3.6-flash"

spec :: Spec
spec = describe "live (needs GEMINI_API_KEY)" $ do
  key <- runIO (lookupEnv "GEMINI_API_KEY")
  case key of
    Nothing -> it "generateContent" $ pendingWith "GEMINI_API_KEY not set"
    Just k -> it "generateContent returns one candidate and usage" $ do
      mgr <- newTlsManager
      logEnv <- initLogEnv "genai-test" "test"
      model <- maybe defaultTestModel T.pack <$> lookupEnv "GEMINI_TEST_MODEL"
      let env = Env mgr geminiApi (ApiKey (T.pack k)) logEnv
          content = mkContent {contentParts = [mkPart {partText = Just "Reply with the single word pong"}], contentRole = Just "user"}
          body = mkGenerateContentRequest [content] model
      r <- runRequest env (generateContent model body)
      case r of
        Left e -> expectationFailure (show e)
        Right resp -> do
          length (generateContentResponseCandidates resp) `shouldBe` 1
          fmap usageMetadataTotalTokenCount (generateContentResponseUsageMetadata resp)
            `shouldSatisfy` maybe False (> 0)
