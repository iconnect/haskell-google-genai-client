# haskell-google-genai-client

Haskell client for Google's Gemini models, maintained by iconnect. Fork of
[sonowz/haskell-google-genai-client](https://github.com/sonowz/haskell-google-genai-client)
(MIT), which is no longer maintained.

Data types and endpoint constructors are **generated** from Google's Discovery
document (`spec/generativelanguage-v1beta.json`); the HTTP runtime is
hand-written and logs through [Katip](https://hackage.haskell.org/package/katip).
Supported GHCs: 9.6.7 and 9.10.3.

Import `GenAI.Client` — it re-exports `GenAI.Client.Types` and
`GenAI.Client.Model` and `GenAI.Client.API` wholesale, plus the curated
entry points from the two hand-written runtime modules: `runRequest` and
`runRequestRaw` from `.Run`, and `UploadSpec` and `uploadFile` from
`.Files`. That covers everything most callers need; import `.Run` or
`.Files` directly for their lower-level machinery (`buildUrl`,
`authHeaders`, `performHttp`, `uploadBase`, ...).

## Backends and auth

| Backend | Constructor | Auth | Endpoints |
|---|---|---|---|
| Gemini Developer API | `geminiApi` | `ApiKey key` (header `x-goog-api-key`) | all generated endpoints |
| Vertex AI | `vertexAi project location` | `BearerToken (IO Text)` | model-scoped endpoints only (see below) |

`vertexAi` derives `https://{location}-aiplatform.googleapis.com/v1`
(`https://aiplatform.googleapis.com/v1` for `global`); override
`backendBaseUrl` if you need another host. Getting the OAuth token is the
caller's job (`gcloud auth print-access-token`, ADC, …).

`generateContent`, `streamGenerateContent`, `countTokens`, `embedContent` and
`batchEmbedContents` are model-scoped: their model argument accepts a bare
name (`gemini-3.6-flash`) or a `models/`-prefixed one, and they work on both
backends. Every other generated endpoint — `getModel`, `listModels`, and all
of `Files` and `CachedContent` — takes the full resource name exactly as
Google returns it (e.g. `files/abc-123`), and is Gemini-API-only: calling one
on `vertexAi` fails locally with `UnsupportedOnBackend`, before any network
I/O. `Files.uploadFile` is likewise Gemini-API-only.

## Example

```haskell
{-# LANGUAGE OverloadedStrings #-}
import GenAI.Client
import Katip (initLogEnv)
import Network.HTTP.Client.TLS (newTlsManager)

main :: IO ()
main = do
  mgr <- newTlsManager
  logEnv <- initLogEnv "genai" "prod"          -- register scribes to see logs
  let env = Env mgr geminiApi (ApiKey "<GEMINI_API_KEY>") logEnv
      content = mkContent { contentParts = [mkPart { partText = Just "What is the capital of Korea?" }]
                          , contentRole = Just "user" }
      body = mkGenerateContentRequest [content] "gemini-3.6-flash"
  r <- runRequest env (generateContent "gemini-3.6-flash" body)
  case r of
    Left err -> print err
    Right resp -> do
      mapM_ (print . candidateContent) (generateContentResponseCandidates resp)
      print (fmap usageMetadataTotalTokenCount (generateContentResponseUsageMetadata resp))
```

Uploading a file (Gemini API only):

```haskell
Right file <- uploadFile env UploadSpec { uploadMimeType = "video/mp4", uploadDisplayName = Just "lecture", uploadBytes = bytes }
-- poll getFile (fileName file) until fileState file == Just FileStateActive
```

## How optionality works

Google's Discovery document has no `required`/`nullable`; the API speaks
proto3 JSON, which omits fields at their default value. The generator applies:

- fields marked `Required.` are positional arguments of `mkFoo` and are never
  `Maybe` — but they still **decode leniently**: an absent key yields the
  proto3 default (`0`, `""`, `False`, the `*_UNSPECIFIED` enum value) rather
  than failing to parse. They always **encode**, even at that default value.
- lists and maps are never `Maybe` (absent decodes as empty); a *required*
  list or map always encodes, even when empty, while an optional one is
  omitted from the JSON when empty.
- nested messages are `Maybe` and decode with `.:?` — except `Required.`
  nested messages, which decode strictly with `.:` because there is nothing
  sane to default a whole sub-message to.
- in **response-only** types (e.g. `UsageMetadata`, `Candidate`, `Model`)
  every scalar defaults when absent, `Required.` or not — a response you
  didn't construct can't have a `Maybe` to distinguish "absent" from "zero".
- in types that also appear in a request (e.g. `GenerationConfig`) non-required
  scalars are `Maybe`, so `temperature = Just 0` is distinguishable from
  leaving it unset;
- every enum has a `<Type>Unknown Text` constructor: a value newer than the
  vendored spec revision never breaks decoding.

## Errors

`runRequest` and `uploadFile` return `Either GenAIError a`. `GenAIError`:

- `HttpError HttpException` — connection/transport failure.
- `ApiError { apiStatus, apiGoogleStatus, apiRawBody }` — a non-2xx HTTP
  response; `apiGoogleStatus` is the parsed `{"error": {...}}` body, if the
  response had that shape.
- `DecodeError Text ByteString` — a 2xx body that didn't decode as expected.
- `UnsupportedOnBackend Text` — a Gemini-API-only endpoint (Files,
  CachedContent, `getModel`/`listModels`) called against `vertexAi`.
- `UnsupportedOperation Text` — an operation this client doesn't implement,
  e.g. `streamGenerateContent`'s `alt=sse` streaming.
- `MalformedBackendUrl Text` — `uploadFile`'s base URL has no path segment
  to insert `upload/` before (e.g. a bare host).

## Logging

Every request logs at Debug (method, URL, status, duration) and errors at
Error, through the `Katip` `LogEnv` in `Env`; a `LogEnv` with no scribes is
silent. Request/response **bodies and credentials are never logged**. Logged
URLs deliberately drop the query string — not just to be terse, but because
the Files resumable-upload URL (`x-goog-upload-url`) carries a session token
in its query string that must not end up in logs.

## Regenerating from a newer spec

```sh
./scripts/update-spec.sh                                   # fetch, prints revision
cabal run -f codegen genai-codegen -- --spec spec/generativelanguage-v1beta.json --lib lib --tests tests
cabal run -f codegen genai-codegen -- --check --spec spec/generativelanguage-v1beta.json
cabal build all && cabal test test:tests
git diff --stat                                             # review, then commit spec + generated files together
```

To expose more endpoints, add their Discovery method id to `allowlist` in
`codegen/Main.hs` (bare verbs — those not going through `{+model}` — also
need an entry in `fnNames`, `codegen/Analyse.hs`, to get a readable function
name instead of the raw Discovery verb). Regenerate and commit
`lib/GenAI/Client/Model.hs`, `lib/GenAI/Client/API.hs` and
`tests/Instances.hs` together with the spec; none of the three is
hand-edited.

## Migrating from 0.1.x

0.2.0 is a deliberate breaking rewrite — see `CHANGELOG.md`. There is no
compatibility shim; every item below needs a call-site change.

- **179 of 394 generated fields are no longer `Maybe`** (see "How optionality
  works" above). Every `fromMaybe` or lens-based `Maybe`-unwrap at a call
  site needs revisiting — some fields you used to default now just have a
  value:

  ```haskell
  tokensUsed :: GenerateContentResponse -> Maybe Int
  tokensUsed = fmap usageMetadataTotalTokenCount . generateContentResponseUsageMetadata
  -- usageMetadataTotalTokenCount :: UsageMetadata -> Int, not Maybe Int:
  -- UsageMetadata is response-only, so every scalar on it defaults on
  -- decode instead of needing an explicit absent/zero distinction.
  ```

- **`GenAI.Client.Core`, `.Client`, `.MimeTypes`, `.ModelLens`, and the
  generated lenses are gone.** Replacements:

  | 0.1.x | 0.2.0 |
  |---|---|
  | `GenAI.Client.Core` (`GenAIClientConfig`, `newConfig`, `addAuthMethod`, `withStdoutLogging`, auth-method typeclasses) | `GenAI.Client.Types`: `Env`, `Backend`, `Auth` |
  | `GenAI.Client.Client` (`dispatchLbs`, manual response decoding) | `GenAI.Client.Run`: `runRequest`, `runRequestRaw` |
  | `GenAI.Client.MimeTypes` (`ContentType`, `Accept`, multipart helpers) | not needed — `GenAI.Client.Files.uploadFile` drives the resumable upload protocol itself |
  | `GenAI.Client.ModelLens` / generated `microlens` lenses | plain record field accessors and record update on `Model.hs`'s types (see the `## Example` section above) |

- **`dispatchLbs`/`newConfig`/`addAuthMethod`/`withStdoutLogging` are gone.**
  Build an `Env` once (manager, `Backend`, `Auth`, Katip `LogEnv`) and pass
  it to `runRequest` per call — see `## Example` above.

- **Logging is Katip-only.** The old `UseKatip` cabal flag choosing between
  Katip and `monad-logger` is removed; there is nothing to opt into or out
  of, only a Katip `LogEnv` on `Env` (see `## Logging` above).

- **File upload no longer shells out to `curl`.** `GenAI.Client.Files.uploadFile`
  performs the two-step resumable upload (start, then upload+finalize)
  itself; see `## Example` above.

## Not (yet) supported

Streaming (`streamGenerateContent` is generated but `runRequest` rejects
`alt=sse`), retries, credential acquisition, Vertex `cachedContents`/files,
tuned models, semantic retrieval, batches.

## Tests

`cabal test test:tests` runs wire, URL, JSON round-trip and fixture tests.
Set `GEMINI_API_KEY` to also run one live `generateContent` call; without it
that test reports pending, not failing.
