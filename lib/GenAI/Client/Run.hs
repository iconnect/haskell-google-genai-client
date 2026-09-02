{-# LANGUAGE OverloadedStrings #-}

-- | Executes 'Request's against a 'Backend'. Hand-written.
module GenAI.Client.Run
  ( runRequest
  , runRequestRaw
  , buildUrl
  , stripModelsPrefix
  , authHeaders
  , performHttp
  , googleStatusOf
  , redactedRequest
  , redactedUri
  , sanitiseHttpException
  ) where

import Control.Exception (try)
import Data.Aeson (Value (..), decode, encode)
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (parseJSON, parseMaybe)
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import GHC.Clock (getMonotonicTime)
import Katip (LogContexts, Severity (..), logFM, logStr, runKatipContextT)
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Types

import GenAI.Client.Types

-- | Pure: the full URL (with query string) a request resolves to on a backend.
buildUrl :: Backend -> Request a -> Either GenAIError Text
buildUrl backend req = do
  path <- case (backend, reqResource req) of
    (GeminiApi base, ModelMethod m verb) ->
      Right (base <> "/models/" <> stripModelsPrefix m <> verb)
    (GeminiApi base, RawPath p) -> Right (base <> "/" <> p)
    (VertexAi base project location, ModelMethod m verb) ->
      Right
        ( base <> "/projects/" <> project <> "/locations/" <> location
            <> "/publishers/google/models/" <> stripModelsPrefix m <> verb
        )
    (VertexAi {}, RawPath p) -> Left (UnsupportedOnBackend p)
  pure (path <> renderQ (reqQuery req))
  where
    renderQ [] = ""
    renderQ q = decodeUtf8 (renderQuery True [(encodeUtf8 k, Just (encodeUtf8 v)) | (k, v) <- q])

-- | @models/gemini-2.5-flash@ → @gemini-2.5-flash@. Anything else untouched.
stripModelsPrefix :: Text -> Text
stripModelsPrefix m = fromMaybe m (T.stripPrefix "models/" m)

authHeaders :: Auth -> IO [Header]
authHeaders (ApiKey k) = pure [("x-goog-api-key", encodeUtf8 k)]
authHeaders (BearerToken getToken) = do
  t <- getToken
  pure [(hAuthorization, "Bearer " <> encodeUtf8 t)]
authHeaders NoAuth = pure []

-- | Decodes @{"error": {...}}@ from a non-2xx body, if it has that shape.
googleStatusOf :: LBS.ByteString -> Maybe GoogleStatus
googleStatusOf body = case decode body of
  Just (Object o) | Just err <- KM.lookup "error" o -> parseMaybe parseJSON err
  _ -> Nothing

-- | Marks credential-bearing headers so http-client's 'Show' instance for
-- 'HTTP.Request' redacts them. http-client only redacts @Authorization@ by
-- default, which misses our @x-goog-api-key@ header — and an
-- 'HTTP.HttpException' embeds the request verbatim, so an unredacted
-- request would leak the API key into any log of that exception.
redactedRequest :: HTTP.Request -> HTTP.Request
redactedRequest r = r {HTTP.redactHeaders = Set.fromList ["Authorization", "x-goog-api-key"]}

-- | A request's URI with the query string (and fragment) dropped, for
-- logging: scheme, host and path only. Header redaction alone isn't enough
-- -- a URL can carry a secret in its query string that never touches a
-- header, as the Files upload's @x-goog-upload-url@ does (an upload-session
-- token). This strips the query for every request, not just the upload one,
-- so any future URL-borne secret is covered too; the query parameters we do
-- send ourselves (@pageSize@, @updateMask@, ...) are worth losing from the
-- logs for that guarantee -- they are still on the request itself and in
-- any returned error.
redactedUri :: HTTP.Request -> Text
redactedUri = T.takeWhile (\c -> c /= '?' && c /= '#') . T.pack . show . HTTP.getUri

-- | Blanks any URL-borne secret carried *inside* an 'HTTP.HttpException'
-- itself, as opposed to one we log or embed ourselves. 'redactedRequest'
-- and 'redactedUri' cover the request we build and the URL we log, but an
-- 'HTTP.HttpException' thrown by http-client (from 'HTTP.httpLbs', or from
-- 'HTTP.parseRequest' on a malformed URL) carries its own request/URL
-- verbatim, query string included -- so a transport or parse failure on
-- the Files upload's @x-goog-upload-url@ (session token in the query
-- string) would otherwise leak that token both into the log line and into
-- the 'GenAIError' returned to the caller. Used by 'performHttp' and by
-- 'GenAI.Client.Files.uploadFile'.
sanitiseHttpException :: HTTP.HttpException -> HTTP.HttpException
sanitiseHttpException (HTTP.HttpExceptionRequest req content) =
  HTTP.HttpExceptionRequest (redactedRequest req) {HTTP.queryString = ""} content
sanitiseHttpException (HTTP.InvalidUrlException url reason) =
  HTTP.InvalidUrlException (T.unpack (T.takeWhile (\c -> c /= '?' && c /= '#') (T.pack url))) reason

-- | Runs one prepared HTTP request: catches 'HTTP.HttpException', logs
-- method/URL/status/duration at Debug, errors at Error, maps non-2xx to 'ApiError'.
-- Bodies are never logged. The logged URL omits its query string; see
-- 'redactedUri'. A caught exception is passed through 'sanitiseHttpException'
-- before it is shown or returned as 'HttpError', so it can't leak a
-- URL-borne secret either.
performHttp :: Env -> HTTP.Request -> IO (Either GenAIError (HTTP.Response LBS.ByteString))
performHttp env hreq0 = do
  let hreq = redactedRequest hreq0
  t0 <- getMonotonicTime
  res <- try (HTTP.httpLbs hreq (envManager env))
  t1 <- getMonotonicTime
  let ms = T.pack (show (round ((t1 - t0) * 1000) :: Int)) <> "ms"
      desc = decodeUtf8 (HTTP.method hreq) <> " " <> redactedUri hreq
  case res of
    Left e0 -> do
      let e = sanitiseHttpException e0
      logAt ErrorS (desc <> " failed: " <> T.pack (show e))
      pure (Left (HttpError e))
    Right resp -> do
      let st = statusCode (HTTP.responseStatus resp)
      logAt DebugS (desc <> " -> " <> T.pack (show st) <> " " <> ms)
      if st >= 200 && st < 300
        then pure (Right resp)
        else do
          let gs = googleStatusOf (HTTP.responseBody resp)
          logAt ErrorS (desc <> " -> " <> T.pack (show st) <> maybe "" ((": " <>) . googleStatusStatus) gs)
          pure (Left (ApiError st gs (HTTP.responseBody resp)))
  where
    logAt sev msg =
      runKatipContextT (envLogEnv env) (mempty :: LogContexts) "genai" (logFM sev (logStr msg))

-- | Runs a request and returns the raw HTTP response (for header access).
runRequestRaw :: Env -> Request a -> IO (Either GenAIError (HTTP.Response LBS.ByteString))
runRequestRaw env req
  | Just alt <- reqAlt req = pure (Left (UnsupportedOperation ("alt=" <> alt)))
  | otherwise = case buildUrl (envBackend env) req of
      Left e -> pure (Left e)
      Right url -> do
        auth <- authHeaders (envAuth env)
        parsed <- try (HTTP.parseRequest (T.unpack url))
        case parsed of
          Left e -> pure (Left (HttpError e))
          Right r0 ->
            performHttp
              env
              r0
                { HTTP.method = reqMethod req
                , HTTP.requestHeaders =
                    [(hAccept, "application/json"), (hUserAgent, "haskell-google-genai-client/0.2.0")]
                      ++ auth
                      ++ [(hContentType, "application/json") | isJust (reqBody req)]
                , HTTP.requestBody = HTTP.RequestBodyLBS (maybe mempty encode (reqBody req))
                }

-- | Runs a request and decodes the body with the request's decoder.
runRequest :: Env -> Request a -> IO (Either GenAIError a)
runRequest env req = do
  r <- runRequestRaw env req
  pure $ case r of
    Left e -> Left e
    Right resp -> case reqDecode req (HTTP.responseBody resp) of
      Left err -> Left (DecodeError (T.pack err) (HTTP.responseBody resp))
      Right a -> Right a
