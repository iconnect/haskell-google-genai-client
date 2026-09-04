{-# LANGUAGE OverloadedStrings #-}

module Wire (spec) where

import Data.Aeson (decode, encode)
import Data.Int (Int64)
import Network.URI (URI, parseURI, uriToString)
import Test.Hspec

import GenAI.Client.Files (uploadBase)
import GenAI.Client.Types

spec :: Spec
spec = describe "wire helpers" $ do
  it "I64 decodes JSON strings and numbers" $ do
    decode "\"123\"" `shouldBe` Just (I64 123)
    decode "123" `shouldBe` Just (I64 123)
    decode "\"12x\"" `shouldBe` (Nothing :: Maybe I64)
    decode "1.5" `shouldBe` (Nothing :: Maybe I64)
  it "I64 rejects a decimal string that overflows Int64 instead of wrapping" $
    decode "\"99999999999999999999\"" `shouldBe` (Nothing :: Maybe I64)
  it "I64 round-trips the Int64 boundary values" $ do
    decode (encode (I64 (maxBound :: Int64))) `shouldBe` Just (I64 maxBound)
    decode (encode (I64 (minBound :: Int64))) `shouldBe` Just (I64 minBound)
  it "I64 encodes as a string (proto3 JSON)" $
    encode (I64 5) `shouldBe` "\"5\""
  it "Base64Bytes round-trips" $
    decode (encode (Base64Bytes "hello")) `shouldBe` Just (Base64Bytes "hello")
  it "Base64Bytes accepts the URL-safe alphabet" $
    decode "\"-_8=\"" `shouldBe` (decode "\"+/8=\"" :: Maybe Base64Bytes)
  it "Base64Bytes rejects invalid base64 instead of fabricating bytes" $
    decode "\"!!!not base64!!!\"" `shouldBe` (Nothing :: Maybe Base64Bytes)
  it "GoogleStatus tolerates missing fields" $
    decode "{\"code\":429}" `shouldBe` Just (GoogleStatus 429 "" "" [])
  it "query helpers render and drop Nothing" $ do
    qInt "pageSize" (Just 3) `shouldBe` Just ("pageSize", "3")
    qBool "x" (Just True) `shouldBe` Just ("x", "true")
    qText "t" Nothing `shouldBe` Nothing
  it "vertexAi derives the regional and global hosts" $ do
    backendBaseUrl (vertexAi (VertexProject "p") (VertexLocation "europe-west1"))
      `shouldBe` "https://europe-west1-aiplatform.googleapis.com/v1"
    backendBaseUrl (vertexAi (VertexProject "p") (VertexLocation "global"))
      `shouldBe` "https://aiplatform.googleapis.com/v1"
  it "backendFromUrl reads a Gemini proxy base, dropping the trailing slash" $
    case backendFromUrl "https://example.test/proxy/v1beta/" of
      Right b@GeminiApi {} -> backendBaseUrl b `shouldBe` "https://example.test/proxy/v1beta"
      other -> expectationFailure ("expected GeminiApi, got " <> show other)
  it "backendFromUrl reads project and location out of a Vertex URL" $ do
    -- A full endpoint URL parses to the same backend vertexAi derives.
    case backendFromUrl "https://europe-west1-aiplatform.googleapis.com/v1/projects/p/locations/europe-west1/publishers/google/models/gemini-2.5-flash:generateContent" of
      Right b -> b `shouldBe` vertexAi (VertexProject "p") (VertexLocation "europe-west1")
      Left e -> expectationFailure ("unexpected error: " <> show e)
    case backendFromUrl "http://localhost:8080/v1/projects/p/locations/global" of
      Right b@(VertexAi _ project location) -> do
        backendBaseUrl b `shouldBe` "http://localhost:8080/v1"
        (project, location) `shouldBe` (VertexProject "p", VertexLocation "global")
      other -> expectationFailure ("expected VertexAi, got " <> show other)
  it "backendFromUrl rejects what it cannot route" $
    mapM_
      rejects
      [ "https://example.test" -- no path segment
      , "https://example.test/" -- still no path segment
      , "gopher://host/path" -- not http(s)
      , "/v1beta" -- relative
      , "https://example.test/v1beta?x=1" -- query
      , "https://example.test/v1beta#frag" -- fragment
      , "https://host/v1/project/p/locations/l" -- Vertex shape with a typo, not a Gemini proxy
      , "https://host/v1/projects/p" -- Vertex shape cut short
      , "https://host/v1/projects//locations/l" -- empty project
      ]
  it "uploadBase inserts the upload/ segment before the version" $ do
    uploadBase (uri "https://generativelanguage.googleapis.com/v1beta")
      `rendersAs` "https://generativelanguage.googleapis.com/upload/v1beta"
    uploadBase (uri "https://example.test/proxy/v1beta")
      `rendersAs` "https://example.test/proxy/upload/v1beta"
  it "uploadBase tolerates a trailing slash on the base" $
    uploadBase (uri "https://generativelanguage.googleapis.com/v1beta/")
      `rendersAs` "https://generativelanguage.googleapis.com/upload/v1beta"
  it "uploadBase rejects a host with no path segment instead of guessing" $
    case uploadBase (uri "https://example.test") of
      Left (MalformedBackendUrl u) -> u `shouldBe` "https://example.test"
      other -> expectationFailure ("expected MalformedBackendUrl, got " <> show other)
  where
    rejects u = case backendFromUrl u of
      Left (MalformedBackendUrl u') -> u' `shouldBe` u
      other -> expectationFailure ("expected MalformedBackendUrl for " <> show u <> ", got " <> show other)
    rendersAs r expected = case r of
      Right u -> uriToString id u "" `shouldBe` expected
      Left e -> expectationFailure ("unexpected error: " <> show e)

-- | Test-only: 'uploadBase' takes the already-parsed URI a 'Backend' holds.
uri :: String -> URI
uri s = maybe (error ("test URI does not parse: " <> s)) id (parseURI s)
