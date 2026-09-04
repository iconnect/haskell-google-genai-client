{-# LANGUAGE OverloadedStrings #-}

module Files (spec) where

import Data.Either (isLeft)
import Katip (initLogEnv)
import qualified Network.HTTP.Client as HTTP
import Test.Hspec

import Network.URI (parseURI)

import GenAI.Client.Files (UploadSpec (..), uploadFile)
import GenAI.Client.Types

spec :: Spec
spec = describe "uploadFile" $
  it "returns Left, not an exception, when the base URL is malformed enough to fail requestFromURI" $ do
    -- withBaseUrl would reject the gopher scheme, but the constructor is
    -- exported, so a Backend can still carry one; uploadBase accepts any
    -- scheme with a path segment, and only HTTP.requestFromURI trips once
    -- the upload/ segment has been spliced in.
    Just gopher <- pure (parseURI "gopher://host/path")
    mgr <- HTTP.newManager HTTP.defaultManagerSettings
    logEnv <- initLogEnv "genai-test" "test"
    let env = Env mgr (GeminiApi gopher) NoAuth logEnv
        upload = UploadSpec {uploadMimeType = "text/plain", uploadDisplayName = Nothing, uploadBytes = "x"}
    result <- uploadFile env upload
    result `shouldSatisfy` isLeft
