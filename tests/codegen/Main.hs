{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Test.Hspec

import Analyse
import Discovery hiding (Param)

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

  describe "Analyse.fieldOf" $ do
    let field sid dir pname = do
          let s = docSchemas doc Map.! sid
              p = schemaProperties s Map.! pname
          either fail (pure . fst) (fieldOf dir sid pname p)
        shape f = (fType f, fWire f, fPresence f, fPositional f)
    it "Required. string → positional Text" $
      shape <$> field "Ping" Bidirectional "model" `shouldReturn` ("Text", WPlain, Required, True)
    it "optional string in a bidirectional schema stays Maybe" $
      shape <$> field "Ping" Bidirectional "text" `shouldReturn` ("Maybe Text", WPlain, Optional, False)
    it "float in a bidirectional schema stays Maybe (explicit presence)" $
      shape <$> field "Ping" Bidirectional "temperature" `shouldReturn` ("Maybe Double", WPlain, Optional, False)
    it "arrays are never Maybe" $
      shape <$> field "Ping" Bidirectional "tags" `shouldReturn` ("[Text]", WPlain, Mono, False)
    it "message refs are Maybe" $
      shape <$> field "Pong" ResponseOnly "usage" `shouldReturn` ("Maybe Usage", WPlain, Optional, False)
    it "int32 in a response-only schema defaults to 0" $
      shape <$> field "Pong" ResponseOnly "count" `shouldReturn` ("Int", WPlain, Defaulted "0", False)
    it "int64 uses the I64 wire newtype" $
      shape <$> field "Pong" ResponseOnly "big" `shouldReturn` ("Int64", WInt64, Defaulted "0", False)
    it "enum in a response-only schema defaults to its UNSPECIFIED constructor" $
      shape <$> field "Pong" ResponseOnly "state"
        `shouldReturn` ("PongState", WPlain, Defaulted "PongStateStateUnspecified", False)
    it "enum in a bidirectional schema is Maybe" $
      shape <$> field "Pong" Bidirectional "state" `shouldReturn` ("Maybe PongState", WPlain, Optional, False)
    it "timestamps are Maybe even when response-only" $
      shape <$> field "Pong" ResponseOnly "createTime" `shouldReturn` ("Maybe UTCTime", WPlain, Optional, False)
    it "structs are Object" $
      shape <$> field "Pong" ResponseOnly "meta" `shouldReturn` ("Object", WPlain, Mono, False)
    it "produces the enum definition" $ do
      let p = schemaProperties (docSchemas doc Map.! "Pong") Map.! "state"
      fmap snd (fieldOf ResponseOnly "Pong" "state" p)
        `shouldBe` Right [EnumDef "PongState" "Optional. Output only. State."
                            [("PongStateStateUnspecified", "STATE_UNSPECIFIED", "Default."), ("PongStateActive", "ACTIVE", "Live.")]]
    it "names" $ do
      hsFieldName "Pong" "createTime" `shouldBe` "pongCreateTime"
      hsFieldName "File" "display_name" `shouldBe` "fileDisplayName"
      enumTypeName "Candidate" "finishReason" `shouldBe` "CandidateFinishReason"
      enumCtor "CandidateFinishReason" "FINISH_REASON_UNSPECIFIED" `shouldBe` "CandidateFinishReasonFinishReasonUnspecified"
      enumCtor "Foo" "1D" `shouldBe` "FooX1d"
    it "camel preserves a leading underscore but still splits internal separators" $ do
      camel "_responseJsonSchema" `shouldBe` "_responseJsonSchema"
      camel "display_name" `shouldBe` "displayName"
      camel "a_b-c" `shouldBe` "aBC"
      hsFieldName "GenerationConfig" "_responseJsonSchema"
        `shouldNotBe` hsFieldName "GenerationConfig" "responseJsonSchema"

  describe "Analyse.fieldOf enum zero-value detection is case-insensitive" $ do
    let enumProp vs d = Property (Just "string") Nothing Nothing Nothing vs [] Nothing d False
        shape = fmap (\(f, _) -> (fType f, fPresence f))
    it "a lowercase zero value (e.g. \"unspecified\") still defaults, not Maybe" $
      shape (fieldOf ResponseOnly "X" "tier" (enumProp ["unspecified", "standard"] "Tier."))
        `shouldBe` Right ("XTier", Defaulted "XTierUnspecified")
    it "no zero value at all keeps the conservative Maybe fallback" $
      shape (fieldOf ResponseOnly "X" "level" (enumProp ["ALPHA", "BETA"] "Level."))
        `shouldBe` Right ("Maybe XLevel", Optional)

  describe "Analyse.typeDefOf" $
    it "orders fields alphabetically and collects enums" $ do
      td <- either fail pure (typeDefOf dirs (docSchemas doc Map.! "Pong"))
      map fJson (tFields td) `shouldBe` ["big", "count", "createTime", "meta", "shared", "state", "usage"]
      map enumName (tEnums td) `shouldBe` ["PongState"]
      tDoc td `shouldBe` "A pong response."

  describe "Analyse.typeDefOf field-name collisions" $
    it "two JSON keys that camelise to the same field name make the schema fail closed" $ do
      let mkProp d = Property (Just "string") Nothing Nothing Nothing [] [] Nothing d False
          dup =
            Schema
              "Dup"
              "Has a collision."
              (Map.fromList [("displayName", mkProp "One."), ("display_name", mkProp "Two.")])
      case typeDefOf Map.empty dup of
        Left err -> do
          err `shouldContain` "Dup"
          err `shouldContain` "displayName"
          err `shouldContain` "display_name"
        Right _ -> expectationFailure "expected a Left for colliding field names"

  describe "Analyse.endpointOf" $ do
    let ep ident = either fail pure (endpointOf (docMethods doc Map.! ident))
    it "model-scoped method" $ do
      e <- ep "generativelanguage.models.ping"
      epName e `shouldBe` "ping"
      epHttp e `shouldBe` "POST"
      epResource e `shouldBe` TplModel ":ping"
      epPathParams e `shouldBe` ["model"]
      epQuery e `shouldBe` []
      epBody e `shouldBe` Just "Ping"
      epResponse e `shouldBe` Just "Pong"
      epAlt e `shouldBe` Nothing
    it "list with a query record" $ do
      e <- ep "generativelanguage.models.list"
      epName e `shouldBe` "listModels"
      epResource e `shouldBe` TplRaw [Lit "models"]
      epQuery e `shouldBe` [QueryParam "pageSize" "listModelsPageSize" "Int"]
      epBody e `shouldBe` Nothing
    it "delete returning Empty" $ do
      e <- ep "generativelanguage.files.delete"
      epName e `shouldBe` "deleteFile"
      epHttp e `shouldBe` "DELETE"
      epResource e `shouldBe` TplRaw [Param "name"]
      epPathParams e `shouldBe` ["name"]
      epResponse e `shouldBe` Nothing

  describe "Analyse.analyse" $ do
    it "builds the plan from an allowlist" $ do
      plan <- either fail pure (analyse ["generativelanguage.models.ping"] ["generativelanguage.files.delete"] doc)
      planRevision plan `shouldBe` "20260101"
      map tName (planTypes plan) `shouldBe` ["Empty", "Ping", "Pong", "Shared", "Usage"]
      map epName (planEndpoints plan) `shouldBe` ["ping"]
    it "rejects unknown method ids" $
      either (const True) (const False) (analyse ["generativelanguage.models.nope"] [] doc) `shouldBe` True
    it "rejects type-name collisions" $ do
      -- A schema named "PongState" collides with the enum type generated for Pong.state.
      let refProp = Property Nothing Nothing (Just "PongState") Nothing [] [] Nothing "" False
          addRef s = s {schemaProperties = Map.insert "ps" refProp (schemaProperties s)}
          schemas' =
            Map.adjust addRef "Pong" $
              Map.insert "PongState" (Schema "PongState" "" Map.empty) (docSchemas doc)
      either (const True) (const False) (analyse ["generativelanguage.models.ping"] [] doc {docSchemas = schemas'})
        `shouldBe` True
