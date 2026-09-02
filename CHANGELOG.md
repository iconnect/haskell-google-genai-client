# Changelog

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
