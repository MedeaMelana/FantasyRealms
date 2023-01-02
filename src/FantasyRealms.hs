{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module FantasyRealms where

import Data.Boolean (notB, (&&*), (||*))
import Data.Function ((&))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Monoid (Sum (..))
import Data.Set (Set)
import Data.Set qualified as Set

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
  | Candle
  | Necromancer
  | Warhorse
  | SwordOfKeth
  | ShieldOfKeth
  | GemOfOrder
  | BookOfChanges
  | Princess
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

mkCard :: Suit -> String -> Int -> Card
mkCard suit name baseStrength =
  Card {bonus = const 0, penalty = const 0, ..}

withBonusWhen :: Int -> (Hand -> Bool) -> Card -> Card
withBonusWhen score matches = withBonus (when matches score)

withBonus :: Modifier -> Card -> Card
withBonus score card =
  card {bonus = \hand -> bonus card hand + score hand}

withPenalty :: Modifier -> Card -> Card
withPenalty penalty card = card {penalty}

isValid :: Hand -> Bool
isValid hand = Set.size hand == requiredSize
  where
    requiredSize = if Set.member Necromancer hand then 8 else 7

hasCardThat :: (Card -> Bool) -> Hand -> Bool
hasCardThat matches = any (matches . describe)

when :: (Hand -> Bool) -> Int -> Modifier
when matches score hand = if matches hand then score else 0

hasSuit :: Suit -> Card -> Bool
hasSuit wantedSuit card = suit card == wantedSuit

hasName :: CardName -> Card -> Bool
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
  Necromancer ->
    mkCard Wizard "Necromancer" 3
      & withBonusWhen 14 (hasCardThat (hasSuit Leader ||* hasSuit Wizard))
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
      & withBonus (\hand -> 8 * Set.size (Set.filter (otherLeader . describe) hand))
    where
      otherLeader card = hasSuit Leader card && name card /= name (describe Princess)

score :: Hand -> Map CardName Int
score hand =
  Map.fromList
    [ (cardName, scoreCard (describe cardName))
      | cardName <- Set.toList hand
    ]
  where
    scoreCard card = baseStrength card + bonus card hand + penalty card hand
