{-# LANGUAGE OverloadedStrings #-}

module Files (spec) where

import Data.Either (isLeft)
import Katip (initLogEnv)
import qualified Network.HTTP.Client as HTTP
import Test.Hspec

import GenAI.Client.Files (UploadSpec (..), uploadFile)
import GenAI.Client.Types

spec :: Spec
spec = describe "uploadFile" $
  it "returns Left, not an exception, when the base URL is malformed enough to fail parseRequest" $ do
    -- uploadBase accepts any scheme with a path segment after the host, so
    -- a non-http(s) scheme like "gopher" sails past it and only trips
    -- HTTP.parseRequest once the upload/ segment has been spliced in.
    mgr <- HTTP.newManager HTTP.defaultManagerSettings
    logEnv <- initLogEnv "genai-test" "test"
    let env = Env mgr (GeminiApi "gopher://host/path") NoAuth logEnv
        upload = UploadSpec {uploadMimeType = "text/plain", uploadDisplayName = Nothing, uploadBytes = "x"}
    result <- uploadFile env upload
    result `shouldSatisfy` isLeft
