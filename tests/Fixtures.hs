{-# LANGUAGE OverloadedStrings #-}

module Fixtures (spec) where

import Data.Aeson (FromJSON, eitherDecode)
import qualified Data.ByteString.Lazy as LBS
import Test.Hspec

import GenAI.Client.Model
import GenAI.Client.Run (googleStatusOf)
import GenAI.Client.Types

load :: FilePath -> IO LBS.ByteString
load f = LBS.readFile ("tests/fixtures/" <> f)

decodeOrFail :: FromJSON a => FilePath -> IO a
decodeOrFail f = load f >>= either (fail . ((f <> ": ") <>)) pure . eitherDecode

usage :: GenerateContentResponse -> IO UsageMetadata
usage r = maybe (fail "no usageMetadata") pure (generateContentResponseUsageMetadata r)

spec :: Spec
spec = describe "fixtures" $ do
  it "basic generateContent: absent counts read as 0, lists as []" $ do
    r <- decodeOrFail "generate_content_basic.json"
    length (generateContentResponseCandidates r) `shouldBe` 1
    u <- usage r
    usageMetadataPromptTokenCount u `shouldBe` 7
    usageMetadataCandidatesTokenCount u `shouldBe` 8
    usageMetadataTotalTokenCount u `shouldBe` 15
    usageMetadataCachedContentTokenCount u `shouldBe` 0
    usageMetadataThoughtsTokenCount u `shouldBe` 0
    usageMetadataCacheTokensDetails u `shouldBe` []
    map modalityTokenCountTokenCount (usageMetadataPromptTokensDetails u) `shouldBe` [7]
    map candidateFinishReason (generateContentResponseCandidates r) `shouldBe` [CandidateFinishReasonStop]
    generateContentResponseModelVersion r `shouldBe` "gemini-2.0-flash-001"
  it "thinking response exposes thoughtsTokenCount" $ do
    r <- decodeOrFail "generate_content_thinking.json"
    u <- usage r
    usageMetadataThoughtsTokenCount u `shouldBe` 1213
    usageMetadataTotalTokenCount u `shouldBe` 1226
  it "unknown finish reason does not fail decoding" $ do
    r <- decodeOrFail "generate_content_unknown_finish_reason.json"
    map candidateFinishReason (generateContentResponseCandidates r)
      `shouldBe` [CandidateFinishReasonUnknown "SOMETHING_NEW"]
  it "error bodies decode to GoogleStatus" $ do
    e400 <- googleStatusOf <$> load "error_400.json"
    fmap googleStatusCode e400 `shouldBe` Just 400
    fmap googleStatusStatus e400 `shouldBe` Just "INVALID_ARGUMENT"
    fmap (length . googleStatusDetails) e400 `shouldBe` Just 1
    e429 <- googleStatusOf <$> load "error_429.json"
    fmap googleStatusMessage e429 `shouldBe` Just "Resource has been exhausted (e.g. check quota)."
  it "file: int64 string, enum, base64" $ do
    f <- decodeOrFail "file.json"
    fileSizeBytes f `shouldBe` Just 12345
    fileState f `shouldBe` Just FileStateActive
    fileSha256Hash f `shouldBe` Just (Base64Bytes "hello")
    fileName f `shouldBe` Just "files/abc-123"
  it "list models" $ do
    r <- decodeOrFail "list_models.json"
    map modelName (listModelsResponseModels r) `shouldBe` ["models/gemini-2.5-flash"]
    map modelTopK (listModelsResponseModels r) `shouldBe` [64]
    -- baseModelId is Required. and absent from the fixture: pins the
    -- lenient-decoding rule (proto3 default, not a parse failure).
    map modelBaseModelId (listModelsResponseModels r) `shouldBe` [""]
    listModelsResponseNextPageToken r `shouldBe` ""
