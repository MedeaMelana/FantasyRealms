{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

-- | Scoring calculator for the card game
-- [Fantasy Realms](https://boardgamegeek.com/boardgame/223040/fantasy-realms).
module FantasyRealms
  ( Suit (..),
    CardName (..),
    Hand,
    Card (..),
    initializeHand,
    computeEffects,
    scoreHand,
  )
where

import Control.Applicative (liftA2)
import Data.Boolean (Boolean (false), notB, (&&*), (||*))
import Data.List (nub, sort)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

-- Change type
-- BONUS: Clears the Penalty on XXX
-- PENALTY: Blanked
-- PENALTY: Blanks

-- | The suit of a card.
data Suit
  = Army
  | Artifact
  | Beast
  | Flame
  | Flood
  | Land
  | Leader
  | Weapon
  | Weather
  | Wild
  | Wizard
  deriving (Eq, Ord, Enum, Show, Read)

-- | The available cards in the game.
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

-- | A hand of cards and their properties.
type Hand = Map CardName Card

data Card = Card
  { name :: String,
    baseStrength :: Int,
    suit :: Suit,
    bonusScore :: Hand -> Int,
    penaltyScore :: Hand -> Int,
    clearsPenalty :: Card -> Bool
  }

type Predicate a = a -> Bool

hasCardThat :: Predicate Card -> Predicate Hand
hasCardThat = any

pointsWhen :: Int -> Predicate Hand -> Hand -> Int
pointsWhen score matches hand = if matches hand then score else 0

pointsForEachCardThat :: Int -> Predicate Card -> Hand -> Int
pointsForEachCardThat score matches hand = score * Map.size (Map.filter matches hand)

hasSuit :: Suit -> Predicate Card
hasSuit wantedSuit card = suit card == wantedSuit

hasName :: CardName -> Predicate Card
hasName cardName card = name (describe cardName) == name card

(<+>) :: (a -> Int) -> (a -> Int) -> a -> Int
(<+>) = liftA2 (+)

infixr 6 <+>

describe :: CardName -> Card
describe = \case
  Basilisk ->
    Card
      { name = "Basilisk",
        suit = Beast,
        baseStrength = 35,
        bonusScore = const 0,
        penaltyScore = const 0,
        clearsPenalty = const False
        -- TODO: BLANKS all Armies, Leaders, and other Beasts
      }
  BellTower ->
    Card
      { name = "Bell Tower",
        suit = Land,
        baseStrength = 8,
        bonusScore = 15 `pointsWhen` hasCardThat (hasSuit Wizard),
        penaltyScore = const 0,
        clearsPenalty = const False
      }
  BookOfChanges ->
    Card
      { name = "Book of Changes",
        suit = Artifact,
        baseStrength = 3,
        bonusScore = const 0, -- TODO
        penaltyScore = const 0,
        clearsPenalty = const False
      }
  Candle ->
    Card
      { name = "Candle",
        suit = Flame,
        baseStrength = 2,
        bonusScore = 100 `pointsWhen` (hasCardThat (hasName BookOfChanges) &&* hasCardThat (hasName BellTower) &&* hasCardThat (hasSuit Wizard)),
        penaltyScore = const 0,
        clearsPenalty = const False
      }
  Cavern ->
    Card
      { name = "Cavern",
        suit = Land,
        baseStrength = 6,
        bonusScore = 25 `pointsWhen` hasCardThat (hasName DwarvishInfantry ||* hasName Dragon),
        penaltyScore = const 0,
        clearsPenalty = (== Weather) . suit
      }
  Dragon ->
    Card
      { name = "Dragon",
        suit = Beast,
        baseStrength = 30,
        bonusScore = const 0,
        penaltyScore = (-40) `pointsWhen` notB (hasCardThat (hasSuit Wizard)),
        clearsPenalty = const False
      }
  DwarvishInfantry ->
    Card
      { name = "Dwarvish Infantry",
        suit = Army,
        baseStrength = 15,
        bonusScore = const 0,
        penaltyScore = (-2) `pointsForEachCardThat` (hasSuit Army &&* notB (hasName DwarvishInfantry)),
        clearsPenalty = const False
      }
  ElvenArchers ->
    Card
      { name = "Elven Archers",
        suit = Army,
        baseStrength = 10,
        bonusScore = 5 `pointsWhen` notB (hasCardThat (hasSuit Weather)),
        penaltyScore = const 0,
        clearsPenalty = const False
      }
  Empress ->
    Card
      { name = "Empress",
        suit = Leader,
        baseStrength = 15,
        bonusScore = 10 `pointsForEachCardThat` hasSuit Army,
        penaltyScore =
          (-5) `pointsForEachCardThat` (hasSuit Leader &&* notB (hasName Empress)),
        clearsPenalty = const False
      }
  Enchantress ->
    Card
      { name = "Enchantress",
        suit = Wizard,
        baseStrength = 5,
        bonusScore = 5 `pointsForEachCardThat` hasElementalSuit,
        penaltyScore = const 0,
        clearsPenalty = const False
      }
    where
      hasElementalSuit = hasSuit Land ||* hasSuit Weather ||* hasSuit Flood ||* hasSuit Flame
  GemOfOrder ->
    Card
      { name = "Gem of Order",
        suit = Artifact,
        baseStrength = 5,
        bonusScore = scoreLength . longestRunLength . baseStrengths,
        penaltyScore = const 0,
        clearsPenalty = const False
      }
    where
      baseStrengths :: Map k Card -> [Int]
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
    Card
      { name = "Hydra",
        suit = Beast,
        baseStrength = 12,
        bonusScore = 28 `pointsWhen` hasCardThat (hasName Swamp),
        penaltyScore = const 0,
        clearsPenalty = const False
      }
  King ->
    Card
      { name = "King",
        suit = Leader,
        baseStrength = 8,
        bonusScore = \hand -> (perArmyBonus hand `pointsForEachCardThat` hasSuit Army) hand,
        penaltyScore = const 0,
        clearsPenalty = const False
      }
    where
      perArmyBonus hand = if Map.member Queen hand then 20 else 5
  Knights ->
    Card
      { name = "Knights",
        suit = Army,
        baseStrength = 20,
        bonusScore = const 0,
        penaltyScore = (-8) `pointsWhen` notB (hasCardThat (hasSuit Leader)),
        clearsPenalty = const False
      }
  LightCavalry ->
    Card
      { name = "Light Cavalry",
        suit = Army,
        baseStrength = 17,
        bonusScore = const 0,
        penaltyScore = (-2) `pointsForEachCardThat` hasSuit Land,
        clearsPenalty = const False
      }
  Necromancer ->
    Card
      { name = "Necromancer",
        suit = Wizard,
        baseStrength = 3,
        bonusScore = 14 `pointsWhen` hasCardThat (hasSuit Leader ||* hasSuit Wizard),
        penaltyScore = const 0,
        clearsPenalty = const False
      }
  Princess ->
    Card
      { name = "Princess",
        suit = Leader,
        baseStrength = 2,
        bonusScore = 8 `pointsForEachCardThat` (hasSuit Army ||* hasSuit Wizard ||* (hasSuit Leader &&* notB (hasName Princess))),
        penaltyScore = const 0,
        clearsPenalty = const False
      }
  ProtectionRune ->
    Card
      { name = "Protection Rune",
        suit = Artifact,
        baseStrength = 1,
        bonusScore = const 0,
        penaltyScore = const 0,
        clearsPenalty = const True
      }
  Queen ->
    Card
      { name = "Queen",
        suit = Leader,
        baseStrength = 6,
        bonusScore = \hand -> (perArmyBonus hand `pointsForEachCardThat` hasSuit Army) hand,
        penaltyScore = const 0,
        clearsPenalty = const False
      }
    where
      perArmyBonus hand = if Map.member King hand then 20 else 5
  Rangers ->
    Card
      { name = "Rangers",
        suit = Army,
        baseStrength = 5,
        bonusScore = 10 `pointsForEachCardThat` hasSuit Land,
        penaltyScore = const 0,
        clearsPenalty = const False
        -- TODO: CLEARS the word Army from all Penalties.
      }
  ShieldOfKeth ->
    Card
      { name = "Shield of Keth",
        suit = Artifact,
        baseStrength = 4,
        bonusScore =
          15
            `pointsWhen` (hasCardThat (hasSuit Leader) &&* notB (hasCardThat (hasName SwordOfKeth)))
            <+> 40
            `pointsWhen` (hasCardThat (hasSuit Leader) &&* hasCardThat (hasName SwordOfKeth)),
        penaltyScore = const 0,
        clearsPenalty = const False
      }
  Swamp ->
    Card
      { name = "Swamp",
        suit = Flood,
        baseStrength = 18,
        bonusScore = const 0,
        penaltyScore = (-3) `pointsForEachCardThat` (hasSuit Army ||* hasSuit Flame),
        clearsPenalty = const False
      }
  SwordOfKeth ->
    Card
      { name = "Sword of Keth",
        suit = Weapon,
        baseStrength = 7,
        bonusScore =
          10
            `pointsWhen` (hasCardThat (hasSuit Leader) &&* notB (hasCardThat (hasName ShieldOfKeth)))
            <+> 40
            `pointsWhen` (hasCardThat (hasSuit Leader) &&* hasCardThat (hasName ShieldOfKeth)),
        penaltyScore = const 0,
        clearsPenalty = const False
      }
  Unicorn ->
    Card
      { name = "Unicorn",
        suit = Beast,
        baseStrength = 9,
        bonusScore =
          30
            `pointsWhen` hasCardThat (hasName Princess)
            <+> 15
            `pointsWhen` ( notB (hasCardThat (hasName Princess))
                             &&* hasCardThat (hasName Empress ||* hasName Queen ||* hasName Enchantress)
                         ),
        penaltyScore = const 0,
        clearsPenalty = const False
      }
  Warhorse ->
    Card
      { name = "Warhorse",
        suit = Beast,
        baseStrength = 6,
        bonusScore = 14 `pointsWhen` hasCardThat (hasSuit Leader ||* hasSuit Wizard),
        penaltyScore = const 0,
        clearsPenalty = const False
      }
  Wildfire ->
    Card
      { name = "Wildfire",
        suit = Flame,
        baseStrength = 40,
        bonusScore = const 0,
        penaltyScore = const 0,
        clearsPenalty = const False
        -- TODO: BLANKS all cards except Flames, Wizards, Weather, Weapons,
        -- Artifacts, Mountain, Great Flood, Island, Unicorn and Dragon.
      }
  WorldTree ->
    Card
      { name = "World Tree",
        suit = Artifact,
        baseStrength = 2,
        bonusScore = 50 `pointsWhen` allCardsHaveDifferentSuits,
        penaltyScore = const 0,
        clearsPenalty = const False
      }
    where
      allCardsHaveDifferentSuits hand = length suits == length (nub suits)
        where
          suits = map suit (Map.elems hand)

-- | Expands the given cards to a hand.
--
-- All cards have base properties, and still need to have their effects applied to each other.
initializeHand :: Set CardName -> Hand
initializeHand cardNames =
  Map.fromAscList
    [ (cardName, describe cardName)
      | cardName <- Set.toAscList cardNames
    ]

-- | Compute the various effects that cards have on each other.
computeEffects :: Hand -> Hand
computeEffects = clearPenalties

-- | Score a hand of cards based on their current properties.
scoreHand :: Hand -> Map CardName Int
scoreHand hand = Map.map scoreCard hand
  where
    scoreCard :: Card -> Int
    scoreCard card = baseStrength card + bonusScore card hand + penaltyScore card hand

clearPenalties :: Map CardName Card -> Map CardName Card
clearPenalties hand = Map.map updateCard hand
  where
    updateCard :: Card -> Card
    updateCard card
      | isPenaltyCleared card = clearPenalty card
      | otherwise = card

    isPenaltyCleared :: Card -> Bool
    isPenaltyCleared = foldr ((||*) . clearsPenalty) false (Map.elems hand)

    clearPenalty :: Card -> Card
    clearPenalty card = card {penaltyScore = const 0}

-- | Length of the longest run of consecutive values.
longestRunLength :: [Int] -> Int
longestRunLength = snd . foldr go (Nothing, 0)
  where
    go x = \case
      (Just (hd, hdLen), len)
        | x + 1 == hd -> (Just (x, hdLen + 1), (hdLen + 1) `max` len)
      (_, len) -> (Just (x, 1), 1 `max` len)
