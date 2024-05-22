{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

module FantasyRealms where

import Control.Applicative (liftA2)
import Data.Boolean (notB, (&&*), (||*))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

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
  = BellTower
  | BookOfChanges
  | Candle
  | Empress
  | Enchantress
  | King
  | Necromancer
  | Princess
  | Queen
  | ShieldOfKeth
  | SwordOfKeth
  | Unicorn
  | Warhorse
  deriving (Eq, Ord, Enum, Show, Read)

type Hand = Set CardName

type Modifier = Hand -> Int

noModifier :: Modifier
noModifier _ = 0

data Card = Card
  { name :: String,
    baseStrength :: Int,
    suit :: Suit,
    bonus :: Modifier,
    penalty :: Modifier
  }

type Predicate a = a -> Bool

isValid :: Predicate Hand
isValid hand = Set.size hand == requiredSize
  where
    requiredSize = if Set.member Necromancer hand then 8 else 7

hasCardThat :: Predicate Card -> Predicate Hand
hasCardThat matches = any (matches . describe)

pointsWhen :: Int -> Predicate Hand -> Modifier
pointsWhen score matches hand = if matches hand then score else 0

hasSuit :: Suit -> Predicate Card
hasSuit wantedSuit card = suit card == wantedSuit

hasName :: CardName -> Predicate Card
hasName cardName card = name (describe cardName) == name card

(<+>) :: (a -> Int) -> (a -> Int) -> a -> Int
(<+>) = liftA2 (+)

infixr 6 <+>

describe :: CardName -> Card
describe = \case
  BellTower ->
    Card
      { name = "Bell Tower",
        suit = Land,
        baseStrength = 8,
        bonus = 15 `pointsWhen` hasCardThat (hasSuit Wizard),
        penalty = noModifier
      }
  BookOfChanges ->
    Card
      { name = "Book of Changes",
        suit = Artifact,
        baseStrength = 3,
        bonus = noModifier, -- TODO
        penalty = noModifier
      }
  Candle ->
    Card
      { name = "Candle",
        suit = Flame,
        baseStrength = 2,
        bonus = 100 `pointsWhen` (hasCardThat (hasName BookOfChanges) &&* hasCardThat (hasName BellTower) &&* hasCardThat (hasSuit Wizard)),
        penalty = noModifier
      }
  Empress ->
    Card
      { name = "Empress",
        suit = Leader,
        baseStrength = 15,
        bonus = \hand -> 10 * Set.size (Set.filter (hasSuit Army . describe) hand),
        penalty =
          \hand -> (-5) * Set.size (Set.filter (hasSuit Leader . describe) hand)
          -- TODO Empress should not count itself
      }
  Enchantress ->
    Card
      { name = "Enchantress",
        suit = Wizard,
        baseStrength = 5,
        bonus = \hand -> 10 * Set.size (Set.filter (hasElementalSuit . describe) hand),
        penalty = noModifier
      }
    where
      hasElementalSuit = hasSuit Land ||* hasSuit Weather ||* hasSuit Flood ||* hasSuit Flame
  King ->
    Card
      { name = "King",
        suit = Leader,
        baseStrength = 8,
        bonus = \hand -> perArmyBonus hand * Set.size (Set.filter (hasSuit Army . describe) hand),
        penalty = noModifier
      }
    where
      perArmyBonus hand = if Set.member Queen hand then 20 else 5
  Necromancer ->
    Card
      { name = "Necromancer",
        suit = Wizard,
        baseStrength = 3,
        bonus = 14 `pointsWhen` hasCardThat (hasSuit Leader ||* hasSuit Wizard),
        penalty = noModifier
      }
  Princess ->
    Card
      { name = "Princess",
        suit = Leader,
        baseStrength = 2,
        bonus = \hand -> 8 * Set.size (Set.filter (hasSuit Leader . describe) hand),
        penalty = noModifier
      }
  -- TODO Princess should not count itself
  Queen ->
    Card
      { name = "Queen",
        suit = Leader,
        baseStrength = 6,
        bonus = \hand -> perArmyBonus hand * Set.size (Set.filter (hasSuit Army . describe) hand),
        penalty = noModifier
      }
    where
      perArmyBonus hand = if Set.member King hand then 20 else 5
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
        penalty = noModifier
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
        penalty = noModifier
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
        penalty = noModifier
      }
  Warhorse ->
    Card
      { name = "Warhorse",
        suit = Beast,
        baseStrength = 6,
        bonus = 14 `pointsWhen` hasCardThat (hasSuit Leader ||* hasSuit Wizard),
        penalty = noModifier
      }

scoreHand :: Hand -> Map CardName Int
scoreHand hand =
  Map.fromList
    [ (cardName, scoreCard (describe cardName))
      | cardName <- Set.toList hand
    ]
  where
    scoreCard card = baseStrength card + bonus card hand + penalty card hand
