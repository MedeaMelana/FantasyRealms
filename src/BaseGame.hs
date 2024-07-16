{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module BaseGame (CardName (..)) where

import Data.Boolean (notB, (&&*), (||*))
import Data.Foldable (toList)
import Data.Function ((&))
import Data.List (group, nub, sort)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import FantasyRealms

-- | The cards available in the base game.
data CardName
  = AirElemental
  | Basilisk
  | Beastmaster
  | BellTower
  | Blizzard
  | BookOfChanges
  | Candle
  | Cavern
  | Collector
  | Doppelgänger
  | Dragon
  | DwarvishInfantry
  | EarthElemental
  | ElvenArchers
  | ElvenLongbow
  | Empress
  | Enchantress
  | FireElemental
  | Forest
  | Forge
  | FountainOfLife
  | GemOfOrder
  | GreatFlood
  | Hydra
  | Island
  | King
  | Knights
  | LightCavalry
  | Lightning
  | MagicWand
  | Mirage
  | Mountain
  | Necromancer
  | Princess
  | ProtectionRune
  | Queen
  | Rainstorm
  | Rangers
  | Shapeshifter
  | ShieldOfKeth
  | Smoke
  | Swamp
  | SwordOfKeth
  | Unicorn
  | WarDirigible
  | Warhorse
  | WarlockLord
  | Warlord
  | Warship
  | WaterElemental
  | Whirlwind
  | Wildfire
  | WorldTree
  deriving (Eq, Ord, Enum, Show, Read)

instance FantasyRealms CardName where
  describeCard = withName $ \case
    AirElemental ->
      mkCard Weather 4
        & withBonusScore (15 `pointsForEachOtherCardThat` hasSuit Weather)
    Basilisk ->
      mkCard Beast 35
        & blankingEachCardThat (hasOneOfSuits [Army, Leader])
        & blankingEachOtherCardThat (hasSuit Beast)
    Beastmaster ->
      mkCard Wizard 9
        & withBonusScore (9 `pointsForEachCardThat` hasSuit Beast)
        & clearingPenalty (hasSuit Beast)
    BellTower ->
      mkCard Land 8
        & withBonusScore (15 `pointsWhen` hasCardThat (hasSuit Wizard))
    Blizzard ->
      mkCard Weather 30
        & blankingEachCardThat (hasSuit Flood)
        & withPenaltyScore ((-5) `pointsForEachCardThat` hasOneOfSuits [Army, Leader, Beast, Flame])
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
    Collector ->
      mkCard Wizard 7
        & withBonusScore (const mostSameSuit)
      where
        mostSameSuit :: Hand name -> Int
        mostSameSuit = score . maximum . map length . group . sort . fmap suit . Map.elems

        score :: Int -> Int
        score = \case
          3 -> 10
          4 -> 40
          len | len >= 5 -> 100
          _ -> undefined
    Doppelgänger ->
      -- TODO may duplicate the name, base strength, suit, and penalty BUT NOT BONUS of any one
      -- other card in your hand
      mkCard Wild 0
    Dragon ->
      mkCard Beast 30
        & withPenaltyScore ((-40) `pointsWhen` notB (hasCardThat (hasSuit Wizard)))
    DwarvishInfantry ->
      mkCard Army 15
        & withPenaltyScore ((-2) `pointsForEachOtherCardThat` hasSuit Army)
    EarthElemental ->
      mkCard Land 4
        & withBonusScore (15 `pointsForEachOtherCardThat` hasSuit Land)
    ElvenArchers ->
      mkCard Army 10
        & withBonusScore (5 `pointsWhen` notB (hasCardThat (hasSuit Weather)))
    ElvenLongbow ->
      mkCard Weapon 3
        & withBonusScore (30 `pointsWhen` hasCardThat (hasOneOfNames [ElvenArchers, Warlord, Beastmaster]))
    Empress ->
      mkCard Leader 15
        & withBonusScore (10 `pointsForEachCardThat` hasSuit Army)
        & withPenaltyScore ((-5) `pointsForEachOtherCardThat` hasSuit Leader)
    Enchantress ->
      mkCard Wizard 5
        & withBonusScore (5 `pointsForEachCardThat` hasElementalSuit)
      where
        hasElementalSuit = hasOneOfSuits [Land, Weather, Flood, Flame]
    FireElemental ->
      mkCard Flame 4
        & withBonusScore (15 `pointsForEachOtherCardThat` hasSuit Flame)
    Forest ->
      mkCard Land 7
        & withBonusScore (12 `pointsForEachCardThat` (hasSuit Beast ||* hasName ElvenArchers))
    Forge ->
      mkCard Flame 9
        & withBonusScore (9 `pointsForEachCardThat` hasOneOfSuits [Weapon, Artifact])
    FountainOfLife ->
      mkCard Flood 1
        & withBonusScore (const findStrongest)
      where
        findStrongest = maximum . map baseStrength . filter (hasOneOfSuits suits) . toList
        suits = [Weapon, Flood, Flame, Land, Weather]
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
    GreatFlood ->
      mkCard Flood 32
        & blankingEachCardThat (hasSuit Army ||* Land `except` Mountain ||* Flame `except` Lightning)
      where
        suit `except` name = hasSuit suit &&* notB (hasName name)
    Hydra ->
      mkCard Beast 12
        & withBonusScore (28 `pointsWhen` hasCardThat (hasName Swamp))
    Island ->
      -- TODO CLEARS the Penalty on any one Flood or Flame
      mkCard Flood 14
    King ->
      mkCard Leader 8
        & withBonusScore (\self hand -> (perArmyBonus self hand `pointsForEachCardThat` hasSuit Army) self hand)
      where
        perArmyBonus self hand = if hasCardThat (hasName Queen) self hand then 20 else 5
    Knights ->
      mkCard Army 20
        & withPenaltyScore ((-8) `pointsWhen` notB (hasCardThat (hasSuit Leader)))
    LightCavalry ->
      mkCard Army 17
        & withPenaltyScore ((-2) `pointsForEachCardThat` hasSuit Land)
    Lightning ->
      mkCard Flame 11
        & withBonusScore (30 `pointsWhen` hasCardThat (hasName Rainstorm))
    MagicWand ->
      mkCard Weapon 1
        & withBonusScore (25 `pointsWhen` hasCardThat (hasSuit Wizard))
    Mirage ->
      -- TODO May duplicate the name and suit of any one Army, Land, Weather, Flood or Flame in the
      -- game
      mkCard Wild 0
    Mountain ->
      mkCard Land 9
        & withBonusScore (50 `pointsWhen` (hasCardThat (hasName Smoke) &&* hasCardThat (hasName Wildfire)))
        & clearingPenalty (hasSuit Flood)
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
        & withBonusScore (\self hand -> (perArmyBonus self hand `pointsForEachCardThat` hasSuit Army) self hand)
      where
        perArmyBonus self hand = if hasCardThat (hasName King) self hand then 20 else 5
    Rainstorm ->
      mkCard Weather 8
        & withBonusScore (10 `pointsForEachCardThat` hasSuit Flood)
        & blankingEachCardThat (hasSuit Flame &&* notB (hasName Lightning))
    Rangers ->
      -- TODO: CLEARS the word Army from all Penalties.
      mkCard Army 5
        & withBonusScore (10 `pointsForEachCardThat` hasSuit Land)
    Shapeshifter ->
      -- TODO May duplicate the name and suit of any one Artifact, Leader, Wizard, Weapon or Beast
      -- in the game.
      mkCard Wild 0
    ShieldOfKeth ->
      mkCard Artifact 4
        & withBonusScore (15 `pointsWhen` (hasCardThat (hasSuit Leader) &&* notB (hasCardThat (hasName SwordOfKeth))))
        & withBonusScore (40 `pointsWhen` (hasCardThat (hasSuit Leader) &&* hasCardThat (hasName SwordOfKeth)))
    Smoke ->
      mkCard Weather 27
        & blankedUnless (hasCardThat (hasSuit Flame))
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
    WarDirigible ->
      mkCard Weapon 35
        & blankedUnless (hasCardThat (hasSuit Army))
        & blankedWhen (hasCardThat (hasSuit Weather))
    Warhorse ->
      mkCard Beast 6
        & withBonusScore (14 `pointsWhen` hasCardThat (hasOneOfSuits [Leader, Wizard]))
    WarlockLord ->
      mkCard Wizard 25
        & withPenaltyScore ((-10) `pointsForEachCardThat` hasSuit Leader)
        & withPenaltyScore ((-10) `pointsForEachOtherCardThat` hasSuit Wizard)
    Warlord ->
      mkCard Leader 4
        & withBonusScore (const armyStrengthSum)
      where
        armyStrengthSum :: Hand name -> Int
        armyStrengthSum = sum . fmap baseStrength . filter (hasSuit Army) . toList
    Warship ->
      mkCard Weapon 23
        -- TODO CLEARS the word Army from all Penalties of all Floods
        & blankedUnless (hasCardThat (hasSuit Flood))
    WaterElemental ->
      mkCard Flood 4
        & withBonusScore (15 `pointsForEachOtherCardThat` hasSuit Flood)
    Whirlwind ->
      mkCard Weather 13
        & withBonusScore (40 `pointsWhen` hasCombo)
      where
        hasCombo =
          hasCardThat (hasName Rainstorm)
            &&* hasCardThat (hasName Blizzard ||* hasName GreatFlood)
    Wildfire ->
      mkCard Flame 40
        & blankingEachCardThat (notB isExcepted)
      where
        isExcepted =
          hasOneOfSuits [Flame, Wizard, Weather, Weapon, Artifact]
            ||* hasOneOfNames [Mountain, GreatFlood, Island, Unicorn, Dragon]
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
            penaltyBlanks = \_ _ -> Set.empty,
            bonusClearsPenalty = const False
          }

-- | Length of the longest run of consecutive values.
longestRunLength :: [Int] -> Int
longestRunLength = snd . foldr go (Nothing, 0)
  where
    go x = \case
      (Just (hd, hdLen), len)
        | x + 1 == hd -> (Just (x, hdLen + 1), (hdLen + 1) `max` len)
      (_, len) -> (Just (x, 1), 1 `max` len)
