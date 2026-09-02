module Main (main) where

import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxSize)

import qualified Instances
import qualified Run
import qualified Url
import qualified Wire

-- | The 'modifyMaxSize' cap below is load-bearing: do not raise or remove
-- it. 'scale' in the generated 'Arbitrary' instances (see Instances.hs)
-- bounds recursion DEPTH, but list- and map-valued fields still generate at
-- the ambient QuickCheck size, so 'Schema''s list-of-self fields ('anyOf',
-- 'properties') multiply combinatorially as size grows. At QuickCheck's own
-- default size of 100 the suite hangs (observed: still running after 45s).
-- Size 8 keeps it well clear of that cliff while still exercising real
-- variation (see Instances.hs).
main :: IO ()
main = hspec $ modifyMaxSize (const 8) $ do
  Wire.spec
  Run.spec
  Url.spec
  Instances.roundTripSpecs
