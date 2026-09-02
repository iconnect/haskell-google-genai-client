{-# LANGUAGE OverloadedStrings #-}

-- | Gemini API Files: resumable upload. Hand-written because the upload
-- endpoint is a two-step media protocol, not a plain JSON method.
--
-- The whole payload is held in memory and sent to Google in one chunk --
-- there is no chunked/streaming upload here, so a very large file means a
-- very large in-memory 'Data.ByteString.Lazy.ByteString'.
--
-- A successful upload can still return a 'File' whose @state@ is
-- @PROCESSING@; polling it (e.g. via @GenAI.Client.API.getFile@) until it
-- reaches @ACTIVE@ is the caller's responsibility, not this module's.
--
-- Gemini API backend only: 'uploadFile' fails with 'UnsupportedOnBackend' on
-- 'VertexAi', before any network I\/O.
--
-- See <https://ai.google.dev/gemini-api/docs/files>.
module GenAI.Client.Files
  ( UploadSpec (..)
  , uploadFile
  , uploadBase
  ) where

import Data.Aeson (eitherDecode, encode)
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Types (hContentType)

import GenAI.Client.Model
import GenAI.Client.Run (authHeaders, performHttp)
import GenAI.Client.Types

-- | What to upload: the raw bytes, their MIME type, and an optional display
-- name for the resulting 'File'.
data UploadSpec = UploadSpec
  { uploadMimeType :: !Text
  , uploadDisplayName :: !(Maybe Text)
  , uploadBytes :: !LBS.ByteString
    -- ^ The whole payload; held in memory and sent in one chunk.
  }

-- | @https:\/\/host\/v1beta@ becomes @https:\/\/host\/upload\/v1beta@: the
-- @upload\/@ segment goes immediately before the final (version) path
-- segment, whatever comes before it.
uploadBase :: Text -> Text
uploadBase base = prefix <> "upload/" <> version
  where
    (prefix, version) = T.breakOnEnd "/" base

-- | Uploads bytes via the resumable protocol (start, then upload+finalize)
-- and returns the resulting 'File'. The returned 'File' may still be
-- @PROCESSING@; see the module Haddock.
uploadFile :: Env -> UploadSpec -> IO (Either GenAIError File)
uploadFile env spec = case envBackend env of
  VertexAi {} -> pure (Left (UnsupportedOnBackend "files"))
  GeminiApi base -> do
    auth <- authHeaders (envAuth env)
    let len = B8.pack (show (LBS.length (uploadBytes spec)))
        startBody =
          mkCreateFileRequest
            { createFileRequestFile = Just mkFile {fileDisplayName = uploadDisplayName spec}
            }
    r0 <- HTTP.parseRequest (T.unpack (uploadBase base <> "/files"))
    started <-
      performHttp
        env
        r0
          { HTTP.method = "POST"
          , HTTP.requestHeaders =
              auth
                ++ [ ("X-Goog-Upload-Protocol", "resumable")
                   , ("X-Goog-Upload-Command", "start")
                   , ("X-Goog-Upload-Header-Content-Length", len)
                   , ("X-Goog-Upload-Header-Content-Type", encodeUtf8 (uploadMimeType spec))
                   , (hContentType, "application/json")
                   ]
          , HTTP.requestBody = HTTP.RequestBodyLBS (encode startBody)
          }
    case started of
      Left e -> pure (Left e)
      Right resp -> case lookup "x-goog-upload-url" (HTTP.responseHeaders resp) of
        Nothing -> pure (Left (DecodeError "missing x-goog-upload-url header" (HTTP.responseBody resp)))
        Just uploadUrl -> do
          r1 <- HTTP.parseRequest (B8.unpack uploadUrl)
          finished <-
            performHttp
              env
              r1
                { HTTP.method = "POST"
                , HTTP.requestHeaders =
                    auth
                      ++ [ ("X-Goog-Upload-Offset", "0")
                         , ("X-Goog-Upload-Command", "upload, finalize")
                         ]
                , HTTP.requestBody = HTTP.RequestBodyLBS (uploadBytes spec)
                }
          pure $ case finished of
            Left e -> Left e
            Right resp2 -> case eitherDecode (HTTP.responseBody resp2) of
              Left err -> Left (DecodeError (T.pack err) (HTTP.responseBody resp2))
              Right CreateFileResponse {createFileResponseFile = Just f} -> Right f
              Right _ -> Left (DecodeError "upload response carries no file" (HTTP.responseBody resp2))
