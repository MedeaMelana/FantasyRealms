module Main (main) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import FantasyRealms
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $
  testCase "Scoring hands" $ do
    assertScores []
    assertScores [(BellTower, 23), (Enchantress, 10)]

assertScores :: [(CardName, Int)] -> Assertion
assertScores expectedScores = Map.fromList expectedScores @?= computedScores
  where
    computedScores = scoreHand (Set.fromList (map fst expectedScores))
