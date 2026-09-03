{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Test.Hspec

import qualified Data.Text as T

import Analyse
import Discovery hiding (Param)
import Emit

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
    it "reads a path parameter's pattern (fix round 1)" $ do
      let probe =
            "{\"revision\": \"1\", \"schemas\": {}, \"resources\": {\"things\": {\"methods\": {\"get\": \
            \{\"id\": \"x.get\", \"path\": \"v1/{+name}\", \"httpMethod\": \"GET\", \"parameters\": \
            \{\"name\": {\"type\": \"string\", \"location\": \"path\", \"pattern\": \"^things/[^/]+$\"}}}}}}}"
      case parseDoc probe of
        Left err -> expectationFailure ("parseDoc failed: " ++ err)
        Right probeDoc ->
          paramPattern (methodParams (docMethods probeDoc Map.! "x.get") Map.! "name")
            `shouldBe` Just "^things/[^/]+$"

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
    -- Deliberate semantic change (fix round 1): "lenient decode, strict
    -- encode". A Required. scalar keeps its non-Maybe type and stays
    -- positional in mk*, but decodes with a default instead of failing, since
    -- Discovery's Required. constrains callers, not Google's responses.
    it "Required. string → positional Text, defaulted (lenient decode)" $
      shape <$> field "Ping" Bidirectional "model" `shouldReturn` ("Text", WPlain, Defaulted "\"\"", True)
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

  describe "Analyse.fieldOf: Required. is lenient on decode, strict on encode" $ do
    let req ty extra = extra (Property (Just ty) Nothing Nothing Nothing [] [] Nothing "Required. A thing." False)
        plain p = p
        shape = fmap (\(f, _) -> (fType f, fPresence f, fPositional f))
    it "a required scalar defaults on decode and still always encodes" $ do
      shape (fieldOf Bidirectional "X" "n" (req "integer" plain))
        `shouldBe` Right ("Int", Defaulted "0", True)
      shape (fieldOf Bidirectional "X" "b" (req "boolean" plain))
        `shouldBe` Right ("Bool", Defaulted "False", True)
    it "a required int64 defaults on decode" $
      shape (fieldOf Bidirectional "X" "big" (req "string" (\p -> p {propFormat = Just "int64"})))
        `shouldBe` Right ("Int64", Defaulted "0", True)
    it "a required enum with a zero value defaults on decode" $
      shape (fieldOf Bidirectional "X" "type" (req "string" (\p -> p {propEnum = ["TYPE_UNSPECIFIED", "OBJECT"]})))
        `shouldBe` Right ("XType", Defaulted "XTypeTypeUnspecified", True)
    it "a required list stays Mono and positional" $
      shape (fieldOf Bidirectional "X" "items" (req "array" (\p -> p {propItems = Just (Property (Just "string") Nothing Nothing Nothing [] [] Nothing "" False)})))
        `shouldBe` Right ("[Text]", Mono, True)
    it "a required map stays Mono and positional" $
      shape (fieldOf Bidirectional "X" "response" (req "object" plain))
        `shouldBe` Right ("Object", Mono, True)
    it "a required nested message still decodes strictly" $
      shape (fieldOf Bidirectional "X" "content" (Property Nothing Nothing (Just "Content") Nothing [] [] Nothing "Required. The content." False))
        `shouldBe` Right ("Content", Required, True)
    it "a required timestamp (no sane default) still decodes strictly" $
      shape (fieldOf Bidirectional "X" "at" (req "string" (\p -> p {propFormat = Just "google-datetime"})))
        `shouldBe` Right ("UTCTime", Required, True)
    it "a required enum with no zero value still decodes strictly" $
      shape (fieldOf Bidirectional "X" "level" (req "string" (\p -> p {propEnum = ["ALPHA", "BETA"]})))
        `shouldBe` Right ("XLevel", Required, True)

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

  describe "Emit.emitModel" $ do
    -- One field per Presence x Wire combination the rules can produce, so the
    -- emitted decoder/encoder for each is pinned. This is the guard that would
    -- have caught the <$>/<*> fixity mis-grouping.
    let fld j ty pres wire posl =
          Field
            { fJson = j
            , fName = "m" <> upperFirst j
            , fType = ty
            , fBase = ty
            , fWire = wire
            , fPresence = pres
            , fPositional = posl
            , fDoc = ""
            }
        matrix =
          [ fld "reqMsg" "Sub" Required WPlain True
          , fld "optTxt" "Maybe Text" Optional WPlain False
          , fld "optBig" "Maybe Int64" Optional WInt64 False
          , fld "defTxt" "Text" (Defaulted "\"\"") WPlain False
          , fld "defBig" "Int64" (Defaulted "0") WInt64 False
          , fld "reqTxt" "Text" (Defaulted "\"\"") WPlain True
          , fld "monoList" "[Text]" Mono WPlain False
          , fld "monoMap" "Object" Mono WPlain False
          , fld "reqList" "[Text]" Mono WPlain True
          , fld "monoBig" "[Int64]" Mono WInt64 False
          ]
        out = emitModel (Plan "R" [TypeDef "M" "A thing." matrix []] [] mempty)
        has x = it (T.unpack x) (T.unpack out `shouldContain` T.unpack x)

    it "puts the revision in the header" $
      T.unpack out `shouldContain` "(revision R)"

    describe "decoders (every one parenthesised, against the infixl 4 chain)" $ do
      has "<$> (o .: \"reqMsg\")"
      has "<*> (o .:? \"optTxt\")"
      has "<*> (fmap unI64 <$> o .:? \"optBig\")"
      has "<*> (o .:? \"defTxt\" .!= \"\")"
      has "<*> (maybe 0 unI64 <$> o .:? \"defBig\")"
      has "<*> (o .:? \"reqTxt\" .!= \"\")"
      has "<*> (o .:? \"monoList\" .!= mempty)"
      has "<*> (o .:? \"monoMap\" .!= mempty)"
      has "<*> (o .:? \"reqList\" .!= mempty)"
      has "<*> (map unI64 <$> o .:? \"monoBig\" .!= mempty)"

    describe "encoders" $ do
      has "Just (\"reqMsg\" .= mReqMsg)"
      has "(\"optTxt\" .=) <$> mOptTxt"
      has "(\"optBig\" .=) . I64 <$> mOptBig"
      has "Just (\"defTxt\" .= mDefTxt)"
      has "Just (\"defBig\" .= I64 mDefBig)"
      has "if null mMonoList then Nothing else Just (\"monoList\" .= mMonoList)"
      has "if null mMonoBig then Nothing else Just (\"monoBig\" .= map I64 mMonoBig)"
      it "a Required. collection is emitted even when empty" $ do
        T.unpack out `shouldContain` "Just (\"reqList\" .= mReqList)"
        T.unpack out `shouldNotContain` "if null mReqList"

    it "makes the Required. fields positional in mk*, in field order" $
      T.unpack out `shouldContain` "mkM :: Sub -> Text -> [Text] -> M"

    it "omits the Haddock line entirely when a description is empty" $ do
      let bare = emitModel (Plan "R" [TypeDef "E" "" [] []] [] mempty)
      T.unpack bare `shouldContain` "data E = E"
      T.unpack bare `shouldNotContain` "-- | \n"

  describe "Emit.emitApi (fix round 1)" $ do
    let ep =
          Endpoint
            { epName = "getThing"
            , epHttp = "GET"
            , epPath = "v1beta/{+name}"
            , epPathParams = ["name"]
            , epPathParamDocs =
                [PathParamDoc "name" "Required. The thing. Format: `things/{thing}`" (Just "^things/[^/]+$")]
            , epResource = TplRaw [Param "name"]
            , epQuery = []
            , epBody = Nothing
            , epResponse = Just "Thing"
            , epAlt = Nothing
            , epDoc = "Gets a thing."
            }
        out = T.unpack (emitApi (Plan "R" [] [ep] mempty))

    it "the METHOD/path line is an unescaped Haddock code span" $
      -- Finding 1: this line must NOT go through the escaper, or the '@'/'/'
      -- delimiters that make it a code span would themselves get escaped.
      out `shouldContain` "-- | @GET v1beta/{+name}@"

    it "the free-text description is still escaped" $
      out `shouldContain` "-- Gets a thing."

    it "emits the path parameter's description and pattern" $ do
      -- Finding 2b/2c: the description (free text) is escaped, so its
      -- backtick and slash come back escaped. The pattern is its own code
      -- span, unescaped at the '@' delimiters -- but its own '/' is still
      -- backslash-escaped: a real Haddock render showed that an unescaped
      -- slash *pair* (this pattern has two) is parsed as emphasis even
      -- inside a code span, so the literal render needs the escape.
      out `shouldContain` "-- * @name@: Required. The thing. Format: \\`things\\/{thing}\\`"
      out `shouldContain` "(pattern: @^things\\/[^\\/]+$@)"

    it "omits a path parameter's bullet when it has neither description nor pattern" $ do
      let bareEp = ep {epPathParamDocs = [PathParamDoc "name" "" Nothing]}
          bareOut = T.unpack (emitApi (Plan "R" [] [bareEp] mempty))
      bareOut `shouldNotContain` "-- * @name@"

  describe "Emit.emitInstances (fix round 1)" $ do
    let node =
          Field
            { fJson = "next"
            , fName = "nodeNext"
            , fType = "Maybe Node"
            , fBase = "Node"
            , fWire = WPlain
            , fPresence = Optional
            , fPositional = False
            , fDoc = ""
            }
        colorEnum = EnumDef "Color" "A color." [("ColorRed", "RED", "Red."), ("ColorBlue", "BLUE", "Blue.")]
        plan = Plan "R" [TypeDef "Node" "A self-referential node." [node] [colorEnum]] [] mempty
        out = T.unpack (emitInstances plan)
        propLines = filter ("  prop \"" `T.isPrefixOf`) (T.lines (emitInstances plan))

    it "a multi-field type's instance carries the sized/mk<Name> depth guard" $ do
      out `shouldContain` "instance Arbitrary Node where"
      out `shouldContain` "arbitrary = sized $ \\d ->"
      out `shouldContain` "then pure mkNode"
      out `shouldContain` "else scale (`div` 2) (Node <$> arbitrary)"

    it "an enum generator lists only the known constructors, never Unknown" $ do
      out `shouldContain` "instance Arbitrary Color where"
      out `shouldContain` "arbitrary = elements [ColorRed, ColorBlue]"
      out `shouldNotContain` "ColorUnknown"

    it "emits exactly one prop line per type/enum" $
      propLines `shouldBe` ["  prop \"Node\" (roundTrip @Node)", "  prop \"Color\" (roundTrip @Color)"]
