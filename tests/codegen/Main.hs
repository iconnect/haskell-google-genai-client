{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import Test.Hspec

import Discovery

main :: IO ()
main = hspec $ do
  doc <- runIO (either fail pure . parseDoc =<< LBS.readFile "tests/codegen/mini-discovery.json")

  describe "Discovery.parseDoc" $ do
    it "reads the revision and flattens methods by id" $ do
      docRevision doc `shouldBe` "20260101"
      Map.keys (docMethods doc)
        `shouldBe` [ "generativelanguage.files.delete"
                   , "generativelanguage.models.list"
                   , "generativelanguage.models.ping"
                   ]
    it "reads schema properties" $ do
      let ping = docSchemas doc Map.! "Ping"
      schemaDescription ping `shouldBe` "A ping request."
      Map.keys (schemaProperties ping) `shouldBe` ["model", "shared", "tags", "temperature", "text"]
      propRef (schemaProperties ping Map.! "shared") `shouldBe` Just "Shared"
      propFormat (schemaProperties ping Map.! "temperature") `shouldBe` Just "float"
      fmap propType (propItems (schemaProperties ping Map.! "tags")) `shouldBe` Just (Just "string")
    it "reads enums and additionalProperties" $ do
      let pong = docSchemas doc Map.! "Pong"
      propEnum (schemaProperties pong Map.! "state") `shouldBe` ["STATE_UNSPECIFIED", "ACTIVE"]
      fmap propType (propAdditional (schemaProperties pong Map.! "meta")) `shouldBe` Just (Just "any")
    it "reads method request/response refs and parameters" $ do
      let ping = docMethods doc Map.! "generativelanguage.models.ping"
      methodPath ping `shouldBe` "v1beta/{+model}:ping"
      methodHttp ping `shouldBe` "POST"
      methodRequest ping `shouldBe` Just "Ping"
      methodResponse ping `shouldBe` Just "Pong"
      methodParamOrder ping `shouldBe` ["model"]
      paramLocation (methodParams ping Map.! "model") `shouldBe` "path"
      let list = docMethods doc Map.! "generativelanguage.models.list"
      methodRequest list `shouldBe` Nothing
      paramLocation (methodParams list Map.! "pageSize") `shouldBe` "query"
