{-# LANGUAGE OverloadedStrings #-}

module Url (spec) where

import Data.Aeson (Value)
import Data.Text (Text)
import Network.URI (URI, uriToString)
import Test.Hspec

import GenAI.Client.Run (buildUrl)
import GenAI.Client.Types

gen :: Text -> Request Value
gen model = Request "POST" (ModelMethod model ":generateContent") [] Nothing Nothing decodeJsonBody

raw :: Text -> [(Text, Text)] -> Request Value
raw p q = Request "GET" (RawPath p) q Nothing Nothing decodeJsonBody

right :: Either GenAIError URI -> IO String
right = either (fail . show) (\u -> pure (uriToString id u ""))

spec :: Spec
spec = describe "buildUrl" $ do
  it "gemini api model method" $
    right (buildUrl geminiApi (gen "gemini-2.5-flash"))
      `shouldReturn` "https://generativelanguage.googleapis.com/v1beta/models/gemini-2.5-flash:generateContent"
  it "strips a leading models/" $
    right (buildUrl geminiApi (gen "models/gemini-2.5-flash"))
      `shouldReturn` "https://generativelanguage.googleapis.com/v1beta/models/gemini-2.5-flash:generateContent"
  it "vertex regional" $
    right (buildUrl (vertexAi (VertexProject "p") (VertexLocation "europe-west1")) (gen "gemini-2.5-flash"))
      `shouldReturn` "https://europe-west1-aiplatform.googleapis.com/v1/projects/p/locations/europe-west1/publishers/google/models/gemini-2.5-flash:generateContent"
  it "vertex global" $
    right (buildUrl (vertexAi (VertexProject "p") (VertexLocation "global")) (gen "gemini-2.5-flash"))
      `shouldReturn` "https://aiplatform.googleapis.com/v1/projects/p/locations/global/publishers/google/models/gemini-2.5-flash:generateContent"
  it "vertex rejects raw paths" $
    case buildUrl (vertexAi (VertexProject "p") (VertexLocation "global")) (raw "files/abc" []) of
      Left (UnsupportedOnBackend "files/abc") -> pure ()
      other -> expectationFailure (show other)
  it "renders and percent-encodes the query" $
    right (buildUrl geminiApi (raw "models" [("pageSize", "10"), ("pageToken", "a b")]))
      `shouldReturn` "https://generativelanguage.googleapis.com/v1beta/models?pageSize=10&pageToken=a%20b"
