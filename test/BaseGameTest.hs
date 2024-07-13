module Main (main) where

import BaseGame
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
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
    assertScores [(DwarvishInfantry, 11), (ElvenArchers, 15), (LightCavalry, 17)]
    assertScores [(Empress, 10), (Princess, 18), (Enchantress, 5)]
    assertScores [(Wildfire, 40), (ShieldOfKeth, 4), (King, 0)]
    assertScores [(Basilisk, 35), (Warhorse, 0), (LightCavalry, 0), (Swamp, 18)]

assertScores :: [(CardName, Int)] -> Assertion
assertScores expectedScores = zeroedScores @?= Map.fromList expectedScores
  where
    hand = initializeHand (Set.fromList (map fst expectedScores))
    actualScores = scoreHand (applyEffects hand)
    zeroedScores = fmap (fromMaybe 0) actualScores
