{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
-- {-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module FantasyRealms where

import Data.Boolean (notB, (&&*), (||*))
import Data.Function ((&))
import Data.Map (Map)
import Data.Map qualified as Map
-- import Data.Monoid (Sum (..))
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

data Card = Card
  { name :: String,
    baseStrength :: Int,
    suit :: Suit,
    bonus :: Modifier,
    penalty :: Modifier
  }

type Predicate a = a -> Bool

mkCard :: Suit -> String -> Int -> Card
mkCard suit name baseStrength =
  Card {bonus = const 0, penalty = const 0, ..}

withBonusWhen :: Int -> Predicate Hand -> Card -> Card
withBonusWhen score matches = withBonus (when matches score)

withBonus :: Modifier -> Card -> Card
withBonus score card =
  card {bonus = \hand -> bonus card hand + score hand}

withPenalty :: Modifier -> Card -> Card
withPenalty score card =
  card {penalty = \hand -> penalty card hand + score hand}

isValid :: Predicate Hand
isValid hand = Set.size hand == requiredSize
  where
    requiredSize = if Set.member Necromancer hand then 8 else 7

hasCardThat :: Predicate Card -> Predicate Hand
hasCardThat matches = any (matches . describe)

when :: Predicate Hand -> Int -> Modifier
when matches score hand = if matches hand then score else 0

hasSuit :: Suit -> Predicate Card
hasSuit wantedSuit card = suit card == wantedSuit

hasName :: CardName -> Predicate Card
hasName cardName card = name (describe cardName) == name card

describe :: CardName -> Card
describe = \case
  BellTower ->
    mkCard Land "Bell Tower" 8
      & withBonusWhen 15 (hasCardThat (hasSuit Wizard))
  Candle ->
    mkCard Flame "Candle" 2
      & withBonusWhen
        100
        ( hasCardThat (hasName BookOfChanges)
            &&* hasCardThat (hasName BellTower)
            &&* hasCardThat (hasSuit Wizard)
        )
  Empress ->
    mkCard Leader "Empress" 15
      & withBonus (\hand -> 10 * Set.size (Set.filter (hasSuit Army . describe) hand))
      & withPenalty (\hand -> (-5) * Set.size (Set.filter (hasSuit Leader . describe) hand))
      -- TODO Empress should not count itself
  Enchantress ->
    mkCard Wizard "Enchantress" 5
      & withBonus (\hand -> 10 * Set.size (Set.filter (hasElementalSuit . describe) hand))
    where
      hasElementalSuit = hasSuit Land ||* hasSuit Weather ||* hasSuit Flood ||* hasSuit Flame
  King -> mkCard Leader "King" 8
      & withBonus (\hand -> perArmyBonus hand * Set.size (Set.filter (hasSuit Army . describe) hand))
    where
      perArmyBonus hand = if Set.member Queen hand then 20 else 5
  Necromancer ->
    mkCard Wizard "Necromancer" 3
      & withBonusWhen 14 (hasCardThat (hasSuit Leader ||* hasSuit Wizard))
  Queen ->
    mkCard Leader "Queen" 6
      & withBonus (\hand -> perArmyBonus hand * Set.size (Set.filter (hasSuit Army . describe) hand))
    where
      perArmyBonus hand = if Set.member King hand then 20 else 5
  Unicorn ->
    mkCard Beast "Unicorn" 9
      & withBonusWhen 30 (hasCardThat (hasName Princess))
      & withBonusWhen 15 (notB (hasCardThat (hasName Princess)) &&*
          hasCardThat (hasName Empress ||* hasName Queen ||* hasName Enchantress))
  Warhorse ->
    mkCard Beast "Warhorse" 6
      & withBonusWhen 14 (hasCardThat (hasSuit Leader ||* hasSuit Wizard))
  SwordOfKeth ->
    mkCard Weapon "Sword of Keth" 7
      & withBonusWhen
        10
        ( hasCardThat (hasSuit Leader)
            &&* notB (hasCardThat (hasName ShieldOfKeth))
        )
      & withBonusWhen
        40
        (hasCardThat (hasSuit Leader) &&* hasCardThat (hasName ShieldOfKeth))
  ShieldOfKeth ->
    mkCard Artifact "Shield of Keth" 4
      & withBonusWhen
        15
        ( hasCardThat (hasSuit Leader)
            &&* notB (hasCardThat (hasName SwordOfKeth))
        )
      & withBonusWhen
        40
        (hasCardThat (hasSuit Leader) &&* hasCardThat (hasName SwordOfKeth))
  BookOfChanges -> mkCard Artifact "Book of Changes" 3
  Princess ->
    mkCard Leader "Princess" 2
      & withBonus (\hand -> 8 * Set.size (Set.filter (hasSuit Leader . describe) hand))
      -- TODO Princess should not count itself

scoreHand :: Hand -> Map CardName Int
scoreHand hand =
  Map.fromList
    [ (cardName, scoreCard (describe cardName))
      | cardName <- Set.toList hand
    ]
  where
    scoreCard card = baseStrength card + bonus card hand + penalty card hand
