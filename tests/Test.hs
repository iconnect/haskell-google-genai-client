module Main (main) where

import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxSize)

import qualified Instances
import qualified Run
import qualified Url
import qualified Wire

main :: IO ()
main = hspec $ modifyMaxSize (const 8) $ do
  Wire.spec
  Run.spec
  Url.spec
  Instances.roundTripSpecs
