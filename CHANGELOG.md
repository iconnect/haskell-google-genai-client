# Changelog

## 0.3.0 — unreleased

### Changed (breaking)

- `Backend` no longer has record fields. `GeminiApi` and `VertexAi` carry a
  parsed `network-uri` `URI`, and `VertexAi` takes `VertexProject` and
  `VertexLocation` newtypes instead of bare `Text`. `vertexAi` takes the
  newtypes too.
- `backendBaseUrl` is a total function returning the rendered URL as
  `Text`, not a (partial) record selector. Overriding the base URL by record
  update no longer compiles; use the new `withBaseUrl :: Text -> Backend ->
  Either GenAIError Backend`, which is the only place a URL is parsed and
  fails with `MalformedBackendUrl` on anything that is not absolute http(s)
  with a path segment and no query or fragment.
- `GenAI.Client.Run.buildUrl` returns a `URI`; `GenAI.Client.Files.uploadBase`
  takes and returns one. Request URLs are now built structurally and handed
  to http-client with `requestFromURI` instead of being concatenated and
  re-parsed.

### Internal

- `codegen/Emit.hs` renders with `string-interpolate` templates. Generated
  output is unchanged.

## 0.2.0 — 2026-09-02

A deliberate **breaking rewrite**. There is no compatibility shim; see
README's "Migrating from 0.1.x" section for the call-site changes this
forces.

### Changed

- Types and endpoints are now generated from Google's Discovery document
  with proto3-aware optionality rules; 179 of 394 generated fields are no
  longer `Maybe`. The old OpenAPI-generator-based client is gone.
- `GenAI.Client.Core`, `.Client`, `.MimeTypes`, `.ModelLens` and the
  generated lenses are removed, replaced by `GenAI.Client.Types` (`Env`,
  `Backend`, `Auth`), `GenAI.Client.Run` (`runRequest`, `runRequestRaw`),
  and plain record field accessors on the generated types.
- `dispatchLbs`, `newConfig`, `addAuthMethod` and `withStdoutLogging` are
  gone; construct an `Env` and call `runRequest` instead.
- Logging is Katip-only; the old `UseKatip` cabal flag (Katip vs.
  `monad-logger`) is removed.
- File uploads no longer need a `curl` shell-out: `GenAI.Client.Files.uploadFile`
  drives the resumable upload protocol directly.

### Fixed

- Closed the last credential/token leak path: a transport failure carries
  its `HTTP.Request` (query string included) inside the thrown
  `HttpException`, which could leak the Files upload's session token via a
  logged or returned error. `performHttp` now sanitises every caught
  exception before it is shown or returned.
- `uploadFile` no longer throws on a malformed URL (including Google's own
  upload URL); it returns `Left` as its `IO (Either GenAIError File)`
  signature promises.

### Removed

- Unused `case-insensitive` dependency.
