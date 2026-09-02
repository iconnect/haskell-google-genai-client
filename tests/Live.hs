{-# LANGUAGE OverloadedStrings #-}

module Live (spec) where

import qualified Data.Text as T
import Katip (initLogEnv)
import Network.HTTP.Client.TLS (newTlsManager)
import System.Environment (lookupEnv)
import Test.Hspec

import GenAI.Client

spec :: Spec
spec = describe "live (needs GEMINI_API_KEY)" $ do
  key <- runIO (lookupEnv "GEMINI_API_KEY")
  case key of
    Nothing -> it "generateContent" $ pendingWith "GEMINI_API_KEY not set"
    Just k -> it "generateContent returns one candidate and usage" $ do
      mgr <- newTlsManager
      logEnv <- initLogEnv "genai-test" "test"
      let env = Env mgr geminiApi (ApiKey (T.pack k)) logEnv
          content = mkContent {contentParts = [mkPart {partText = Just "Reply with the single word pong"}], contentRole = Just "user"}
          body = mkGenerateContentRequest [content] "gemini-2.5-flash"
      r <- runRequest env (generateContent "gemini-2.5-flash" body)
      case r of
        Left e -> expectationFailure (show e)
        Right resp -> do
          length (generateContentResponseCandidates resp) `shouldBe` 1
          fmap usageMetadataTotalTokenCount (generateContentResponseUsageMetadata resp)
            `shouldSatisfy` maybe False (> 0)
