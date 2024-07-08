module Main (main) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import FantasyRealms
import Test.Tasty
import Test.Tasty.HUnit
import Data.Maybe (fromMaybe)

main :: IO ()
main = defaultMain $
  testCase "Scoring hands" $ do
    assertScores []
    assertScores [(BellTower, 23), (Enchantress, 10)]
    assertScores [(ProtectionRune, 1), (Dragon, 30)]
    assertScores [(DwarvishInfantry, 11), (ElvenArchers, 15), (LightCavalry, 17)]
    assertScores [(Empress, 10), (Princess, 18), (Enchantress, 5)]
    assertScores [(Wildfire, 40), (ShieldOfKeth, 4), (King, 0)]

assertScores :: [(CardName, Int)] -> Assertion
assertScores expectedScores = Map.fromList expectedScores @?= zeroedScores
  where
    hand = initializeHand (Set.fromList (map fst expectedScores))
    actualScores = scoreHand (computeEffects hand)
    zeroedScores = fmap (fromMaybe 0) actualScores
