{-# LANGUAGE OverloadedStrings #-}

module Run (spec) where

import Data.List (isInfixOf)
import qualified Network.HTTP.Client as HTTP
import Test.Hspec

import GenAI.Client.Run (authHeaders, googleStatusOf, redactedRequest)
import GenAI.Client.Types

spec :: Spec
spec = do
  describe "authHeaders" $ do
    it "ApiKey sends x-goog-api-key" $
      authHeaders (ApiKey "secret") `shouldReturn` [("x-goog-api-key", "secret")]
    it "BearerToken sends Authorization: Bearer" $
      authHeaders (BearerToken (pure "tok")) `shouldReturn` [("Authorization", "Bearer tok")]
    it "NoAuth sends no headers" $
      authHeaders NoAuth `shouldReturn` []

  describe "redactedRequest" $
    it "hides the api key from Show, so a logged HttpException can't leak it" $ do
      req0 <- HTTP.parseRequest "https://example.com"
      let req = req0 {HTTP.requestHeaders = [("x-goog-api-key", "supersecret")]}
      show (redactedRequest req) `shouldNotSatisfy` ("supersecret" `isInfixOf`)

  describe "googleStatusOf" $ do
    it "parses the error object out of a Google error body" $
      googleStatusOf "{\"error\":{\"code\":429,\"status\":\"RESOURCE_EXHAUSTED\",\"message\":\"x\"}}"
        `shouldBe` Just (GoogleStatus 429 "x" "RESOURCE_EXHAUSTED" [])
    it "is Nothing for a JSON object without an error field" $
      googleStatusOf "{}" `shouldBe` Nothing
    it "is Nothing for non-JSON bodies" $
      googleStatusOf "not json" `shouldBe` Nothing
