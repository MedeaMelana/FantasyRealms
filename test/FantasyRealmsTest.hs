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
    assertScores [(ProtectionRune, 1), (Dragon, 30)]

assertScores :: [(CardName, Int)] -> Assertion
assertScores expectedScores = Map.fromList expectedScores @?= actualScores
  where
    hand = initializeHand (Set.fromList (map fst expectedScores))
    actualScores = scoreHand (computeEffects hand)
