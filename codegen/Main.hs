{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (unless)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import System.Environment (getArgs)
import System.Directory (createDirectoryIfMissing)
import System.Exit (die)
import System.FilePath (takeDirectory, (</>))

import Analyse
import Discovery
import Emit

-- | Methods that get an endpoint function. Extend here.
allowlist :: [Text]
allowlist =
  [ "generativelanguage.models.generateContent"
  , "generativelanguage.models.streamGenerateContent"
  , "generativelanguage.models.countTokens"
  , "generativelanguage.models.embedContent"
  , "generativelanguage.models.batchEmbedContents"
  , "generativelanguage.models.list"
  , "generativelanguage.models.get"
  , "generativelanguage.files.get"
  , "generativelanguage.files.list"
  , "generativelanguage.files.delete"
  , "generativelanguage.cachedContents.create"
  , "generativelanguage.cachedContents.get"
  , "generativelanguage.cachedContents.list"
  , "generativelanguage.cachedContents.patch"
  , "generativelanguage.cachedContents.delete"
  ]

-- | Methods whose request/response schemas are emitted without an endpoint
-- function (the upload protocol is hand-written in GenAI.Client.Files).
typesOnly :: [Text]
typesOnly = ["generativelanguage.media.upload"]

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--spec", spec, "--lib", lib, "--tests", tests] -> do
      plan <- loadPlan spec
      write (lib </> "GenAI" </> "Client" </> "Model.hs") (emitModel plan)
      write (lib </> "GenAI" </> "Client" </> "API.hs") (emitApi plan)
      write (tests </> "Instances.hs") (emitInstances plan)
      putStrLn ("generated from revision " <> T.unpack (planRevision plan))
    ["--check", "--spec", spec] -> do
      plan <- loadPlan spec
      let expect =
            [ ("UsageMetadata", ResponseOnly), ("Candidate", ResponseOnly), ("GenerateContentResponse", ResponseOnly)
            , ("ModalityTokenCount", ResponseOnly), ("PromptFeedback", ResponseOnly), ("CitationMetadata", ResponseOnly)
            , ("Content", Bidirectional), ("Part", Bidirectional), ("Blob", Bidirectional), ("FileData", Bidirectional)
            , ("GenerationConfig", Bidirectional), ("Schema", Bidirectional), ("File", Bidirectional), ("CachedContent", Bidirectional)
            ]
          expectedEndpoints = 15 :: Int
          bad = [(n, want, Map.lookup n (planDirections plan)) | (n, want) <- expect, Map.lookup n (planDirections plan) /= Just want]
          endpointCountOk = length (planEndpoints plan) == expectedEndpoints
      if null bad && endpointCountOk
        then putStrLn ("check ok: " <> show (length (planTypes plan)) <> " types, " <> show (length (planEndpoints plan)) <> " endpoints")
        else do
          mapM_ reportMismatch bad
          unless endpointCountOk $
            putStrLn
              ( "endpoint count mismatch: expected " <> show expectedEndpoints
                  <> ", found " <> show (length (planEndpoints plan))
              )
          die "direction check failed"
    _ -> die "usage: genai-codegen --spec FILE --lib DIR --tests DIR | genai-codegen --check --spec FILE"

-- | One line per mismatch: the schema, what the check expected, what
-- 'analyse' actually found (or that the schema is not emitted at all).
reportMismatch :: (Text, Direction, Maybe Direction) -> IO ()
reportMismatch (name, want, found) =
  putStrLn
    ( T.unpack name <> ": expected " <> show want <> ", found "
        <> maybe "not emitted" show found
    )

loadPlan :: FilePath -> IO Plan
loadPlan spec = do
  bytes <- LBS.readFile spec
  doc <- either (die . ("spec parse: " <>)) pure (parseDoc bytes)
  either (die . ("analyse: " <>)) pure (analyse allowlist typesOnly doc)

-- | UTF-8 regardless of locale (descriptions contain non-ASCII). Creates the
-- parent directory, so a fresh checkout or a new @--lib@ target just works.
write :: FilePath -> Text -> IO ()
write path t = do
  createDirectoryIfMissing True (takeDirectory path)
  LBS.writeFile path (LBS.fromStrict (encodeUtf8 t))
