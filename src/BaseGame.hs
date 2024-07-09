{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module BaseGame where

import Data.Boolean (notB, (&&*), (||*))
import Data.Function ((&))
import Data.List (nub, sort)
import Data.Map (Map)
import qualified Data.Map as Map
import FantasyRealms

-- | The cards available in the base game.
data CardName
  = Basilisk
  | BellTower
  | BookOfChanges
  | Candle
  | Cavern
  | Dragon
  | DwarvishInfantry
  | ElvenArchers
  | Empress
  | Enchantress
  | GemOfOrder
  | Hydra
  | King
  | Knights
  | LightCavalry
  | Necromancer
  | Princess
  | ProtectionRune
  | Rangers
  | Queen
  | ShieldOfKeth
  | Swamp
  | SwordOfKeth
  | Unicorn
  | Warhorse
  | Wildfire
  | WorldTree
  deriving (Eq, Ord, Enum, Show, Read)

instance FantasyRealms CardName where
  describeCard = withName $ \case
    Basilisk ->
      -- TODO: BLANKS all Armies, Leaders, and other Beasts
      mkCard Beast 35
    BellTower ->
      mkCard Land 8
        & withBonusScore (15 `pointsWhen` hasCardThat (hasSuit Wizard))
    BookOfChanges ->
      -- TODO You may change the suit of one other card
      mkCard Artifact 3
    Candle ->
      mkCard Flame 2
        & withBonusScore (100 `pointsWhen` hasTrio)
      where
        hasTrio =
          hasCardThat (hasName BookOfChanges)
            &&* hasCardThat (hasName BellTower)
            &&* hasCardThat (hasSuit Wizard)
    Cavern ->
      mkCard Land 6
        & withBonusScore (25 `pointsWhen` hasCardThat (hasName DwarvishInfantry ||* hasName Dragon))
        & clearingPenalty (hasSuit Weather)
    Dragon ->
      mkCard Beast 30
        & withPenaltyScore ((-40) `pointsWhen` notB (hasCardThat (hasSuit Wizard)))
    DwarvishInfantry ->
      mkCard Army 15
        & withPenaltyScore ((-2) `pointsForEachOtherCardThat` hasSuit Army)
    ElvenArchers ->
      mkCard Army 10
        & withBonusScore (5 `pointsWhen` notB (hasCardThat (hasSuit Weather)))
    Empress ->
      mkCard Leader 15
        & withBonusScore (10 `pointsForEachCardThat` hasSuit Army)
        & withPenaltyScore ((-5) `pointsForEachOtherCardThat` hasSuit Leader)
    Enchantress ->
      mkCard Wizard 5
        & withBonusScore (5 `pointsForEachCardThat` hasElementalSuit)
      where
        hasElementalSuit = hasOneOfSuits [Land, Weather, Flood, Flame]
    GemOfOrder ->
      mkCard Artifact 5
        & withBonusScore (\_self -> scoreLength . longestRunLength . baseStrengths)
      where
        baseStrengths :: Map k (Card name) -> [Int]
        baseStrengths = sort . map baseStrength . Map.elems

        scoreLength :: Int -> Int
        scoreLength = \case
          0 -> 0
          1 -> 0
          2 -> 0
          3 -> 10
          4 -> 30
          5 -> 60
          6 -> 100
          _ -> 150
    Hydra ->
      mkCard Beast 12
        & withBonusScore (28 `pointsWhen` hasCardThat (hasName Swamp))
    King ->
      mkCard Leader 8
        & withBonusScore (\self hand -> (perArmyBonus hand `pointsForEachCardThat` hasSuit Army) self hand)
      where
        -- TODO Should check name of card, not key in map
        perArmyBonus hand = if Map.member Queen hand then 20 else 5
    Knights ->
      mkCard Army 20
        & withPenaltyScore ((-8) `pointsWhen` notB (hasCardThat (hasSuit Leader)))
    LightCavalry ->
      mkCard Army 17
        & withPenaltyScore ((-2) `pointsForEachCardThat` hasSuit Land)
    Necromancer ->
      mkCard Wizard 3
    Princess ->
      mkCard Leader 2
        & withBonusScore (8 `pointsForEachCardThat` hasOneOfSuits [Army, Wizard])
        & withBonusScore (8 `pointsForEachOtherCardThat` hasSuit Leader)
    ProtectionRune ->
      mkCard Artifact 1
        & clearingPenalty (const True)
    Queen ->
      mkCard Leader 6
        & withBonusScore (\self hand -> (perArmyBonus hand `pointsForEachCardThat` hasSuit Army) self hand)
      where
        perArmyBonus hand = if Map.member King hand then 20 else 5
    Rangers ->
      -- TODO: CLEARS the word Army from all Penalties.
      mkCard Army 5
        & withBonusScore (10 `pointsForEachCardThat` hasSuit Land)
    ShieldOfKeth ->
      mkCard Artifact 4
        & withBonusScore (15 `pointsWhen` (hasCardThat (hasSuit Leader) &&* notB (hasCardThat (hasName SwordOfKeth))))
        & withBonusScore (40 `pointsWhen` (hasCardThat (hasSuit Leader) &&* hasCardThat (hasName SwordOfKeth)))
    Swamp ->
      mkCard Flood 18
        & withPenaltyScore ((-3) `pointsForEachCardThat` hasOneOfSuits [Army, Flame])
    SwordOfKeth ->
      mkCard Weapon 7
        & withBonusScore (10 `pointsWhen` (hasCardThat (hasSuit Leader) &&* notB (hasCardThat (hasName ShieldOfKeth))))
        & withBonusScore (40 `pointsWhen` (hasCardThat (hasSuit Leader) &&* hasCardThat (hasName ShieldOfKeth)))
    Unicorn ->
      mkCard Beast 9
        & withBonusScore (30 `pointsWhen` hasCardThat (hasName Princess))
        & withBonusScore
          ( 15
              `pointsWhen` ( notB (hasCardThat (hasName Princess))
                               &&* hasCardThat (hasName Empress ||* hasName Queen ||* hasName Enchantress)
                           )
          )
    Warhorse ->
      mkCard Beast 6
        & withBonusScore (14 `pointsWhen` hasCardThat (hasOneOfSuits [Leader, Wizard]))
    Wildfire ->
      -- TODO: BLANKS all cards except Flames, Wizards, Weather, Weapons,
      -- Artifacts, Mountain, Great Flood, Island, Unicorn and Dragon.
      mkCard Flame 40
        & blanking
          ( notB
              ( hasOneOfSuits [Flame, Wizard, Weather, Weapon, Artifact]
                  ||* hasName Unicorn
                  ||* hasName Dragon
              )
          )
    WorldTree ->
      mkCard Artifact 2
        & withBonusScore (50 `pointsWhen` allCardsHaveDifferentSuits)
      where
        allCardsHaveDifferentSuits _self hand = length suits == length (nub suits)
          where
            suits = map suit (Map.elems hand)
    where
      withName :: (CardName -> Card CardName) -> CardName -> Card CardName
      withName f cardName = (f cardName) {name = cardName}

      mkCard :: Suit -> Int -> Card name
      mkCard suit baseStrength =
        Card
          { name = undefined,
            suit,
            baseStrength,
            bonusScore = \_ _ -> 0,
            penaltyScore = \_ _ -> 0,
            penaltyBlanks = const False,
            clearsPenalty = const False
          }

-- | Length of the longest run of consecutive values.
longestRunLength :: [Int] -> Int
longestRunLength = snd . foldr go (Nothing, 0)
  where
    go x = \case
      (Just (hd, hdLen), len)
        | x + 1 == hd -> (Just (x, hdLen + 1), (hdLen + 1) `max` len)
      (_, len) -> (Just (x, 1), 1 `max` len)
