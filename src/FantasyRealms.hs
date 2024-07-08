{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

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
import Data.Function ((&))
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
  { name :: CardName,
    baseStrength :: Int,
    suit :: Suit,
    bonusScore :: CardName -> Hand -> Int,
    penaltyScore :: CardName -> Hand -> Int,
    penaltyBlanks :: Card -> Bool,
    clearsPenalty :: Card -> Bool
  }

withBonusScore :: (CardName -> Hand -> Int) -> Card -> Card
withBonusScore f c@(Card {bonusScore}) = c {bonusScore = bonusScore <+> f}

withPenaltyScore :: (CardName -> Hand -> Int) -> Card -> Card
withPenaltyScore f c@(Card {penaltyScore}) = c {penaltyScore = penaltyScore <+> f}

blanking :: (Card -> Bool) -> Card -> Card
blanking f c@(Card {penaltyBlanks}) = c {penaltyBlanks = penaltyBlanks ||* f}

clearingPenalty :: (Card -> Bool) -> Card -> Card
clearingPenalty f c@(Card {clearsPenalty}) = c {clearsPenalty = clearsPenalty ||* f}

type Predicate a = a -> Bool

hasCardThat :: Predicate Card -> CardName -> Hand -> Bool
hasCardThat matches _ = any matches

hasOtherCardThat :: Predicate Card -> CardName -> Hand -> Bool
hasOtherCardThat matches cardName = Map.foldrWithKey f False
  where
    f self card foundRest = (self /= cardName && matches card) || foundRest

pointsWhen :: Int -> (CardName -> Hand -> Bool) -> CardName -> Hand -> Int
pointsWhen score matches cardName hand = if matches cardName hand then score else 0

pointsForEachCardThat :: Int -> (Card -> Bool) -> CardName -> Hand -> Int
pointsForEachCardThat score matches _ hand = score * Map.size (Map.filter matches hand)

pointsForEachOtherCardThat :: Int -> (Card -> Bool) -> CardName -> Hand -> Int
pointsForEachOtherCardThat score matches cardName hand =
  score * Map.size (Map.filterWithKey otherCardMatches hand)
  where
    otherCardMatches self card = self /= cardName && matches card

hasSuit :: Suit -> Predicate Card
hasSuit wantedSuit card = suit card == wantedSuit

hasName :: CardName -> Predicate Card
hasName cardName card = cardName == name card

(<+>) :: (a -> b -> Int) -> (a -> b -> Int) -> a -> b -> Int
(<+>) = liftA2 (liftA2 (+))

infixr 6 <+>

describe :: CardName -> Card
describe = withName $ \case
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
      hasElementalSuit = hasSuit Land ||* hasSuit Weather ||* hasSuit Flood ||* hasSuit Flame
  GemOfOrder ->
    mkCard Artifact 5
      & withBonusScore (\_self -> scoreLength . longestRunLength . baseStrengths)
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
      & withBonusScore (8 `pointsForEachCardThat` (hasSuit Army ||* hasSuit Wizard))
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
      & withPenaltyScore ((-3) `pointsForEachCardThat` (hasSuit Army ||* hasSuit Flame))
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
      & withBonusScore (14 `pointsWhen` hasCardThat (hasSuit Leader ||* hasSuit Wizard))
  Wildfire ->
    -- TODO: BLANKS all cards except Flames, Wizards, Weather, Weapons,
    -- Artifacts, Mountain, Great Flood, Island, Unicorn and Dragon.
    mkCard Flame 40
      & blanking
        ( notB
            ( hasSuit Flame
                ||* hasSuit Wizard
                ||* hasSuit Weather
                ||* hasSuit Weapon
                ||* hasSuit Artifact
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
    withName :: (CardName -> Card) -> CardName -> Card
    withName f cardName = (f cardName) {name = cardName}

    mkCard :: Suit -> Int -> Card
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

-- | Expands the given cards to a hand.
--
-- All cards have base properties, and still need to have their effects applied to each other.
initializeHand :: Set CardName -> Hand
initializeHand cardNames =
  Map.fromAscList
    [ (cardName, describe cardName)
      | cardName <- Set.toAscList cardNames
    ]

-- | Compute the various effects that cards have on each other, together with
-- whether a card is blanked.
computeEffects :: Hand -> Map CardName (Card, Bool)
computeEffects = blankCards . clearPenalties

blankCards :: Hand -> Map CardName (Card, Bool)
blankCards hand = Map.map updateCard hand
  where
    updateCard card = (card, any (`penaltyBlanks` card) hand)

clearPenalties :: Hand -> Hand
clearPenalties hand = Map.map updateCard hand
  where
    updateCard :: Card -> Card
    updateCard card
      | isPenaltyCleared card = clearPenalty card
      | otherwise = card

    isPenaltyCleared :: Card -> Bool
    isPenaltyCleared = foldr ((||*) . clearsPenalty) false (Map.elems hand)

    clearPenalty :: Card -> Card
    clearPenalty card =
      card
        { penaltyScore = \_ _ -> 0,
          penaltyBlanks = const False
        }

-- | Score a hand of cards based on their current properties.
--
-- Each card is mapped to its individual score, if it is not blanked.
scoreHand :: Map CardName (Card, Bool) -> Map CardName (Maybe Int)
scoreHand handWithBlanks = Map.mapWithKey scoreCard handWithBlanks
  where
    handWithoutBlanks :: Hand
    handWithoutBlanks = fmap fst (Map.filter (not . snd) handWithBlanks)

    scoreCard :: CardName -> (Card, Bool) -> Maybe Int
    scoreCard cardName (card, blanked)
      | blanked = Nothing
      | otherwise =
          Just $
            baseStrength card
              + bonusScore card cardName handWithoutBlanks
              + penaltyScore card cardName handWithoutBlanks

-- | Length of the longest run of consecutive values.
longestRunLength :: [Int] -> Int
longestRunLength = snd . foldr go (Nothing, 0)
  where
    go x = \case
      (Just (hd, hdLen), len)
        | x + 1 == hd -> (Just (x, hdLen + 1), (hdLen + 1) `max` len)
      (_, len) -> (Just (x, 1), 1 `max` len)
