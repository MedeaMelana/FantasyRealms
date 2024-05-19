module Main (main) where

import FantasyRealms
import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Set as Set

main :: IO ()
main = defaultMain $
  testCase "Scoring hands" $ do
    0 @?= sum (scoreHand Set.empty)
