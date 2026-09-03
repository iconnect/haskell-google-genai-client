-- | Everything most callers need. Import this module qualified or as is.
--
-- This is a curated default, not a blanket re-export: 'GenAI.Client.Run'
-- and 'GenAI.Client.Files' also expose lower-level request machinery
-- (@buildUrl@, @authHeaders@, @performHttp@, @uploadBase@, ...) that most
-- callers never need and that can change more freely than this facade's
-- surface. Import those modules directly if you need them.
module GenAI.Client
  ( module GenAI.Client.Types
  , runRequest
  , runRequestRaw
  , UploadSpec (..)
  , uploadFile
  , module GenAI.Client.Model
  , module GenAI.Client.API
  ) where

import GenAI.Client.API
import GenAI.Client.Files (UploadSpec (..), uploadFile)
import GenAI.Client.Model
import GenAI.Client.Run (runRequest, runRequestRaw)
import GenAI.Client.Types
