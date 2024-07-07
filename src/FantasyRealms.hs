{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

module FantasyRealms where

import Control.Applicative (liftA2)
import Data.Boolean (notB, (&&*), (||*))
import Data.List (nub, sort)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import qualified Data.Set as Set

-- Change type
-- BONUS: Clears the Penalty on XXX
-- PENALTY: Blanked
-- PENALTY: Blanks

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

type Hand = Map CardName Card

data Card = Card
  { name :: String,
    baseStrength :: Int,
    suit :: Suit,
    bonus :: Hand -> Int,
    penalty :: Hand -> Int
  }

type Predicate a = a -> Bool

isValid :: Predicate Hand
isValid hand = Map.size hand == requiredSize
  where
    requiredSize = if Map.member Necromancer hand then 8 else 7

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
        bonus = const 0,
        penalty = const 0
        -- TODO: BLANKS all Armies, Leaders, and other Beasts
      }
  BellTower ->
    Card
      { name = "Bell Tower",
        suit = Land,
        baseStrength = 8,
        bonus = 15 `pointsWhen` hasCardThat (hasSuit Wizard),
        penalty = const 0
      }
  BookOfChanges ->
    Card
      { name = "Book of Changes",
        suit = Artifact,
        baseStrength = 3,
        bonus = const 0, -- TODO
        penalty = const 0
      }
  Candle ->
    Card
      { name = "Candle",
        suit = Flame,
        baseStrength = 2,
        bonus = 100 `pointsWhen` (hasCardThat (hasName BookOfChanges) &&* hasCardThat (hasName BellTower) &&* hasCardThat (hasSuit Wizard)),
        penalty = const 0
      }
  Cavern ->
    Card
      { name = "Cavern",
        suit = Land,
        baseStrength = 6,
        bonus = 25 `pointsWhen` hasCardThat (hasName DwarvishInfantry ||* hasName Dragon),
        penalty = const 0
        -- TODO: CLEARS the Penalty on all Weather
      }
  Dragon ->
    Card
      { name = "Dragon",
        suit = Beast,
        baseStrength = 30,
        bonus = const 0,
        penalty = (-40) `pointsWhen` notB (hasCardThat (hasSuit Wizard))
      }
  DwarvishInfantry ->
    Card
      { name = "Dwarvish Infantry",
        suit = Army,
        baseStrength = 15,
        bonus = const 0,
        penalty = (-2) `pointsForEachCardThat` (hasSuit Army &&* notB (hasName DwarvishInfantry))
      }
  ElvenArchers ->
    Card
      { name = "Elven Archers",
        suit = Army,
        baseStrength = 10,
        bonus = 5 `pointsWhen` notB (hasCardThat (hasSuit Weather)),
        penalty = const 0
      }
  Empress ->
    Card
      { name = "Empress",
        suit = Leader,
        baseStrength = 15,
        bonus = 10 `pointsForEachCardThat` hasSuit Army,
        penalty =
          (-5) `pointsForEachCardThat` (hasSuit Leader &&* notB (hasName Empress))
      }
  Enchantress ->
    Card
      { name = "Enchantress",
        suit = Wizard,
        baseStrength = 5,
        bonus = 5 `pointsForEachCardThat` hasElementalSuit,
        penalty = const 0
      }
    where
      hasElementalSuit = hasSuit Land ||* hasSuit Weather ||* hasSuit Flood ||* hasSuit Flame
  GemOfOrder ->
    Card
      { name = "Gem of Order",
        suit = Artifact,
        baseStrength = 5,
        bonus = scoreLength . longestRunLength . baseStrengths,
        penalty = const 0
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
        bonus = 28 `pointsWhen` hasCardThat (hasName Swamp),
        penalty = const 0
      }
  King ->
    Card
      { name = "King",
        suit = Leader,
        baseStrength = 8,
        bonus = \hand -> (perArmyBonus hand `pointsForEachCardThat` hasSuit Army) hand,
        penalty = const 0
      }
    where
      perArmyBonus hand = if Map.member Queen hand then 20 else 5
  Knights ->
    Card
      { name = "Knights",
        suit = Army,
        baseStrength = 20,
        bonus = const 0,
        penalty = (-8) `pointsWhen` notB (hasCardThat (hasSuit Leader))
      }
  LightCavalry ->
    Card
      { name = "Light Cavalry",
        suit = Army,
        baseStrength = 17,
        bonus = const 0,
        penalty = (-2) `pointsForEachCardThat` hasSuit Land
      }
  Necromancer ->
    Card
      { name = "Necromancer",
        suit = Wizard,
        baseStrength = 3,
        bonus = 14 `pointsWhen` hasCardThat (hasSuit Leader ||* hasSuit Wizard),
        penalty = const 0
      }
  Princess ->
    Card
      { name = "Princess",
        suit = Leader,
        baseStrength = 2,
        bonus = 8 `pointsForEachCardThat` (hasSuit Army ||* hasSuit Wizard ||* (hasSuit Leader &&* notB (hasName Princess))),
        penalty = const 0
      }
  ProtectionRune ->
    Card
      { name = "Protection Rune",
        suit = Artifact,
        baseStrength = 1,
        bonus = const 0,
        penalty = const 0
        -- TODO: CLEARS the Penalty on all cards
      }
  Queen ->
    Card
      { name = "Queen",
        suit = Leader,
        baseStrength = 6,
        bonus = \hand -> (perArmyBonus hand `pointsForEachCardThat` hasSuit Army) hand,
        penalty = const 0
      }
    where
      perArmyBonus hand = if Map.member King hand then 20 else 5
  Rangers ->
    Card
      { name = "Rangers",
        suit = Army,
        baseStrength = 5,
        bonus = 10 `pointsForEachCardThat` hasSuit Land,
        penalty = const 0
        -- TODO: CLEARS the word Army from all Penalties.
      }
  ShieldOfKeth ->
    Card
      { name = "Shield of Keth",
        suit = Artifact,
        baseStrength = 4,
        bonus =
          15
            `pointsWhen` (hasCardThat (hasSuit Leader) &&* notB (hasCardThat (hasName SwordOfKeth)))
            <+> 40
            `pointsWhen` (hasCardThat (hasSuit Leader) &&* hasCardThat (hasName SwordOfKeth)),
        penalty = const 0
      }
  Swamp ->
    Card
      { name = "Swamp",
        suit = Flood,
        baseStrength = 18,
        bonus = const 0,
        penalty = (-3) `pointsForEachCardThat` (hasSuit Army ||* hasSuit Flame)
      }
  SwordOfKeth ->
    Card
      { name = "Sword of Keth",
        suit = Weapon,
        baseStrength = 7,
        bonus =
          10
            `pointsWhen` (hasCardThat (hasSuit Leader) &&* notB (hasCardThat (hasName ShieldOfKeth)))
            <+> 40
            `pointsWhen` (hasCardThat (hasSuit Leader) &&* hasCardThat (hasName ShieldOfKeth)),
        penalty = const 0
      }
  Unicorn ->
    Card
      { name = "Unicorn",
        suit = Beast,
        baseStrength = 9,
        bonus =
          30
            `pointsWhen` hasCardThat (hasName Princess)
            <+> 15
            `pointsWhen` ( notB (hasCardThat (hasName Princess))
                             &&* hasCardThat (hasName Empress ||* hasName Queen ||* hasName Enchantress)
                         ),
        penalty = const 0
      }
  Warhorse ->
    Card
      { name = "Warhorse",
        suit = Beast,
        baseStrength = 6,
        bonus = 14 `pointsWhen` hasCardThat (hasSuit Leader ||* hasSuit Wizard),
        penalty = const 0
      }
  Wildfire ->
    Card
      { name = "Wildfire",
        suit = Flame,
        baseStrength = 40,
        bonus = const 0,
        penalty = const 0
        -- TODO: BLANKS all cards except Flames, Wizards, Weather, Weapons,
        -- Artifacts, Mountain, Great Flood, Island, Unicorn and Dragon.
      }
  WorldTree ->
    Card
      { name = "World Tree",
        suit = Artifact,
        baseStrength = 2,
        bonus = 50 `pointsWhen` allCardsHaveDifferentSuits,
        penalty = const 0
      }
    where
      allCardsHaveDifferentSuits hand = length suits == length (nub suits)
        where
          suits = map suit (Map.elems hand)

initializeHand :: Set CardName -> Hand
initializeHand cardNames =
  Map.fromAscList
    [ (cardName, describe cardName)
      | cardName <- Set.toAscList cardNames
    ]

scoreHand :: Hand -> Map CardName Int
scoreHand hand = Map.map scoreCard hand
  where
    scoreCard card = baseStrength card + bonus card hand + penalty card hand

-- | Length of the longest run of consecutive values.
longestRunLength :: [Int] -> Int
longestRunLength = snd . foldr go (Nothing, 0)
  where
    go x = \case
      (Just (hd, hdLen), len)
        | x + 1 == hd -> (Just (x, hdLen + 1), (hdLen + 1) `max` len)
      (_, len) -> (Just (x, 1), 1 `max` len)
