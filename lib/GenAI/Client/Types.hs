{-# LANGUAGE OverloadedStrings #-}

-- | Hand-written core types. Nothing in this module is generated.
module GenAI.Client.Types
  ( -- * Environment
    Env (..)
  , Backend (..)
  , VertexProject (..)
  , VertexLocation (..)
  , geminiApi
  , vertexAi
  , withBaseUrl
  , backendBaseUrl
  , Auth (..)

    -- * Requests
  , Request (..)
  , Resource (..)
  , decodeJsonBody
  , qText
  , qInt
  , qBool

    -- * Errors
  , GenAIError (..)
  , GoogleStatus (..)

    -- * Wire helpers used by generated code
  , Base64Bytes (..)
  , I64 (..)
  ) where

import Control.Exception (Exception)
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Base64.URL as B64U
import qualified Data.ByteString.Lazy as LBS
import Data.Int (Int64)
import Data.List (dropWhileEnd)
import qualified Data.Scientific as Sci
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeLatin1, encodeUtf8)
import qualified Data.Text.Read as TR
import Katip (LogEnv)
import Network.HTTP.Client (HttpException, Manager)
import Network.HTTP.Types (Method)
import Network.URI (URI (..), URIAuth (..), parseAbsoluteURI, uriToString)

-- | A Google Cloud project id, for 'VertexAi'.
newtype VertexProject = VertexProject Text
  deriving (Show, Eq)

-- | A Vertex AI region (@europe-west1@, ...) or @global@.
newtype VertexLocation = VertexLocation Text
  deriving (Show, Eq)

-- | Where requests go. Both constructors carry the base URL up to and
-- including the API version segment, without a trailing slash, query or
-- fragment. Build one with 'geminiApi' or 'vertexAi' and, for a
-- non-standard host, 'withBaseUrl'; the 'URI' inside is an implementation
-- detail callers never need to touch.
data Backend
  = -- | Gemini Developer API. Default base: @https://generativelanguage.googleapis.com/v1beta@.
    GeminiApi !URI
  | -- | Vertex AI. Default base: @https://{location}-aiplatform.googleapis.com/v1@,
    -- or @https://aiplatform.googleapis.com/v1@ when location is @global@.
    VertexAi !URI !VertexProject !VertexLocation
  deriving (Show, Eq)

geminiApi :: Backend
geminiApi = GeminiApi (httpsUri "generativelanguage.googleapis.com" "/v1beta")

vertexAi :: VertexProject -> VertexLocation -> Backend
vertexAi project location@(VertexLocation l) = VertexAi (httpsUri host "/v1") project location
  where
    host
      | l == "global" = "aiplatform.googleapis.com"
      | otherwise = l <> "-aiplatform.googleapis.com"

-- | Built structurally rather than parsed, so the defaults stay total.
httpsUri :: Text -> String -> URI
httpsUri host path =
  URI
    { uriScheme = "https:"
    , uriAuthority = Just (URIAuth "" (T.unpack host) "")
    , uriPath = path
    , uriQuery = ""
    , uriFragment = ""
    }

-- | Replaces a backend's base URL, keeping its kind and Vertex fields. This is
-- the one place a URL is parsed: the input must be an absolute @http@ or
-- @https@ URL with a host and at least one path segment (the API version),
-- and no query or fragment. A trailing slash is dropped. Anything else is
-- 'MalformedBackendUrl'.
withBaseUrl :: Text -> Backend -> Either GenAIError Backend
withBaseUrl url backend = case parseAbsoluteURI (T.unpack url) of
  Just uri
    | uriScheme uri `elem` ["http:", "https:"]
    , Just auth <- uriAuthority uri
    , not (null (uriRegName auth))
    , any (/= '/') path
    , null (uriQuery uri)
    , null (uriFragment uri) ->
        Right (replaceBase uri {uriPath = path})
    where
      path = dropWhileEnd (== '/') (uriPath uri)
  _ -> Left (MalformedBackendUrl url)
  where
    replaceBase uri = case backend of
      GeminiApi _ -> GeminiApi uri
      VertexAi _ project location -> VertexAi uri project location

-- | The base URL a backend sends requests to, rendered.
backendBaseUrl :: Backend -> Text
backendBaseUrl backend = T.pack (uriToString id uri "")
  where
    uri = case backend of
      GeminiApi u -> u
      VertexAi u _ _ -> u

data Auth
  = -- | Sent as the @x-goog-api-key@ header (never in the URL).
    ApiKey !Text
  | -- | Run before every request; the caller owns refresh and caching.
    BearerToken (IO Text)
  | NoAuth

data Env = Env
  { envManager :: !Manager
  , envBackend :: !Backend
  , envAuth :: !Auth
  , envLogEnv :: !LogEnv
  -- ^ Katip environment. A 'LogEnv' with no scribes is silent.
  }

data Resource
  = -- | @ModelMethod model ":verb"@. Routable to both backends.
    ModelMethod !Text !Text
  | -- | Path relative to the base URL, e.g. @files/abc@. Gemini API only.
    RawPath !Text
  deriving (Show, Eq)

data Request a = Request
  { reqMethod :: !Method
  , reqResource :: !Resource
  , reqQuery :: ![(Text, Text)]
  , reqBody :: !(Maybe Value)
  , reqAlt :: !(Maybe Text)
  -- ^ Streaming hook (@alt=sse@). Not supported by 'GenAI.Client.Run.runRequest' yet.
  , reqDecode :: LBS.ByteString -> Either String a
  }

decodeJsonBody :: FromJSON a => LBS.ByteString -> Either String a
decodeJsonBody = eitherDecode

qText :: Text -> Maybe Text -> Maybe (Text, Text)
qText k = fmap (k,)

qInt :: Text -> Maybe Int -> Maybe (Text, Text)
qInt k = fmap (\n -> (k, T.pack (show n)))

qBool :: Text -> Maybe Bool -> Maybe (Text, Text)
qBool k = fmap (\b -> (k, if b then "true" else "false"))

data GenAIError
  = HttpError HttpException
  | ApiError
      { apiStatus :: !Int
      , apiGoogleStatus :: !(Maybe GoogleStatus)
      , apiRawBody :: !LBS.ByteString
      }
  | DecodeError !Text !LBS.ByteString
  | -- | A 'RawPath' request was sent to the 'VertexAi' backend.
    UnsupportedOnBackend !Text
  | -- | e.g. streaming.
    UnsupportedOperation !Text
  | -- | A base URL was rejected: by 'withBaseUrl' (not absolute http(s), or
    -- no path segment, or a query\/fragment), or by 'GenAI.Client.Files'
    -- because it has no path segment to insert @upload\/@ before. Carries
    -- the offending URL.
    MalformedBackendUrl !Text
  deriving (Show)

instance Exception GenAIError

-- | The @error@ object Google returns on non-2xx responses (google.rpc.Status).
data GoogleStatus = GoogleStatus
  { googleStatusCode :: !Int
  , googleStatusMessage :: !Text
  , googleStatusStatus :: !Text
  , googleStatusDetails :: ![Value]
  }
  deriving (Show, Eq)

instance FromJSON GoogleStatus where
  parseJSON = withObject "GoogleStatus" $ \o ->
    GoogleStatus
      <$> o .:? "code" .!= 0
      <*> o .:? "message" .!= ""
      <*> o .:? "status" .!= ""
      <*> o .:? "details" .!= []

-- | proto3 @bytes@: base64 on the wire.
newtype Base64Bytes = Base64Bytes {unBase64Bytes :: ByteString}
  deriving (Show, Eq)

instance ToJSON Base64Bytes where
  toJSON = String . decodeLatin1 . B64.encode . unBase64Bytes

instance FromJSON Base64Bytes where
  parseJSON = withText "Base64Bytes" $ \t ->
    let bs = encodeUtf8 t
     in case B64.decode bs of
          Right decoded -> pure (Base64Bytes decoded)
          Left err -> case B64U.decode bs of
            Right decoded -> pure (Base64Bytes decoded)
            Left _ -> fail ("Base64Bytes: invalid base64: " <> err)

-- | proto3 @int64@: a decimal string on the wire, but numbers are accepted too.
newtype I64 = I64 {unI64 :: Int64}
  deriving (Show, Eq)

instance ToJSON I64 where
  toJSON (I64 n) = String (T.pack (show n))

instance FromJSON I64 where
  parseJSON (String t) = case TR.signed TR.decimal t of
    Right (n, rest) | T.null rest -> boundedI64 n
    _ -> fail ("I64: not an integer string: " <> T.unpack t)
  parseJSON (Number n) =
    maybe (fail "I64: not an integer") (pure . I64) (Sci.toBoundedInteger n)
  parseJSON _ = fail "I64: expected string or number"

-- | Bounds-check an arbitrary-precision integer into 'Int64', rejecting
-- overflow instead of silently wrapping (as 'fromInteger' would).
boundedI64 :: Integer -> Parser I64
boundedI64 n
  | n < toInteger (minBound :: Int64) || n > toInteger (maxBound :: Int64) =
      fail ("I64: integer out of Int64 range: " <> show n)
  | otherwise = pure (I64 (fromInteger n))
