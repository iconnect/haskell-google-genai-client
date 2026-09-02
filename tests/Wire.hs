{-# LANGUAGE OverloadedStrings #-}

module Wire (spec) where

import Data.Aeson (decode, encode)
import Test.Hspec

import GenAI.Client.Types

spec :: Spec
spec = describe "wire helpers" $ do
  it "I64 decodes JSON strings and numbers" $ do
    decode "\"123\"" `shouldBe` Just (I64 123)
    decode "123" `shouldBe` Just (I64 123)
    decode "\"12x\"" `shouldBe` (Nothing :: Maybe I64)
    decode "1.5" `shouldBe` (Nothing :: Maybe I64)
  it "I64 encodes as a string (proto3 JSON)" $
    encode (I64 5) `shouldBe` "\"5\""
  it "Base64Bytes round-trips" $
    decode (encode (Base64Bytes "hello")) `shouldBe` Just (Base64Bytes "hello")
  it "Base64Bytes accepts the URL-safe alphabet" $
    decode "\"-_8=\"" `shouldBe` (decode "\"+/8=\"" :: Maybe Base64Bytes)
  it "GoogleStatus tolerates missing fields" $
    decode "{\"code\":429}" `shouldBe` Just (GoogleStatus 429 "" "" [])
  it "query helpers render and drop Nothing" $ do
    qInt "pageSize" (Just 3) `shouldBe` Just ("pageSize", "3")
    qBool "x" (Just True) `shouldBe` Just ("x", "true")
    qText "t" Nothing `shouldBe` Nothing
  it "vertexAi derives the regional and global hosts" $ do
    backendBaseUrl (vertexAi "p" "europe-west1")
      `shouldBe` "https://europe-west1-aiplatform.googleapis.com/v1"
    backendBaseUrl (vertexAi "p" "global")
      `shouldBe` "https://aiplatform.googleapis.com/v1"
