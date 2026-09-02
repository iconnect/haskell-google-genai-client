{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Test.Hspec

import Analyse
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
    it "yields Nothing for a response object with no $ref, instead of failing the whole parse" $ do
      let probe =
            "{\"revision\": \"1\", \"schemas\": {}, \"resources\": {\"things\": {\"methods\": {\"noop\": \
            \{\"id\": \"x.noop\", \"path\": \"v1/noop\", \"httpMethod\": \"GET\", \"response\": {}}}}}}"
      case parseDoc probe of
        Left err -> expectationFailure ("parseDoc failed: " ++ err)
        Right probeDoc -> methodResponse (docMethods probeDoc Map.! "x.noop") `shouldBe` Nothing

  let methods = Map.elems (docMethods doc)
      dirs = directions doc methods

  describe "Analyse.directions" $ do
    it "emits exactly the reachable closure" $
      Map.keys dirs `shouldBe` ["Empty", "Ping", "Pong", "Shared", "Usage"]
    it "classifies response-only vs bidirectional" $ do
      Map.lookup "Pong" dirs `shouldBe` Just ResponseOnly
      Map.lookup "Usage" dirs `shouldBe` Just ResponseOnly
      Map.lookup "Empty" dirs `shouldBe` Just ResponseOnly
      Map.lookup "Ping" dirs `shouldBe` Just Bidirectional
      Map.lookup "Shared" dirs `shouldBe` Just Bidirectional
    it "detects markers anywhere in the leading sentence group" $ do
      let p d = Property Nothing Nothing Nothing Nothing [] [] Nothing d False
      isOutputOnly (p "Optional. Output only. State.") `shouldBe` True
      isOutputOnly (p "Output only. State.") `shouldBe` True
      isOutputOnly (p "The output only matters later. Output only.") `shouldBe` False
      isRequired (p "Required. Immutable. The model.") `shouldBe` True
      isRequired (p "Optional. Text.") `shouldBe` False
    it "closure follows items and additionalProperties" $ do
      let schemas = docSchemas doc
      closure schemas (const True) (Set.fromList ["Pong"])
        `shouldBe` Set.fromList ["Pong", "Shared", "Usage"]
      closure schemas (not . isOutputOnly) (Set.fromList ["Pong"])
        `shouldBe` Set.fromList ["Pong", "Shared"]
