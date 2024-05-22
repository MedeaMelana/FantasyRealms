module Main (main) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import FantasyRealms
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $
  testCase "Scoring hands" $ do
    0 @?= sum (scoreHand Set.empty)
    Map.fromAscList [(BellTower, 23), (Enchantress, 10)] @?= scoreHand (Set.fromList [BellTower, Enchantress])
