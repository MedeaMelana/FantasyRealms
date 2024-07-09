{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Scoring calculator for the card game
-- [Fantasy Realms](https://boardgamegeek.com/boardgame/223040/fantasy-realms).
module FantasyRealms
  ( FantasyRealms (..),
    Suit (..),
    Hand,
    Card (..),
    withBonusScore,
    withPenaltyScore,
    pointsWhen,
    hasCardThat,
    hasSuit,
    hasName,
    clearingPenalty,
    pointsForEachOtherCardThat,
    pointsForEachCardThat,
    hasOneOfSuits,
    blanking,
    initializeHand,
    computeEffects,
    scoreHand,
  )
where

import Control.Applicative (liftA2)
import Data.Boolean (Boolean (false), (||*))
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

-- | A hand of cards and their properties.
type Hand name = Map name (Card name)

data Card name = Card
  { name :: name,
    baseStrength :: Int,
    suit :: Suit,
    bonusScore :: name -> Hand name -> Int,
    penaltyScore :: name -> Hand name -> Int,
    penaltyBlanks :: Card name -> Bool,
    clearsPenalty :: Card name -> Bool
  }

class FantasyRealms name where
  describeCard :: name -> Card name

withBonusScore :: (name -> Hand name -> Int) -> Card name -> Card name
withBonusScore f c@(Card {bonusScore}) = c {bonusScore = bonusScore <+> f}

withPenaltyScore :: (name -> Hand name -> Int) -> Card name -> Card name
withPenaltyScore f c@(Card {penaltyScore}) = c {penaltyScore = penaltyScore <+> f}

blanking :: (Card name -> Bool) -> Card name -> Card name
blanking f c@(Card {penaltyBlanks}) = c {penaltyBlanks = penaltyBlanks ||* f}

clearingPenalty :: (Card name -> Bool) -> Card name -> Card name
clearingPenalty f c@(Card {clearsPenalty}) = c {clearsPenalty = clearsPenalty ||* f}

type Predicate a = a -> Bool

hasCardThat :: Predicate (Card name) -> name -> Hand name -> Bool
hasCardThat matches _ = any matches

hasOtherCardThat :: (Eq name) => Predicate (Card name) -> name -> Hand name -> Bool
hasOtherCardThat matches cardName = Map.foldrWithKey f False
  where
    f self card foundRest = (self /= cardName && matches card) || foundRest

pointsWhen :: Int -> (name -> Hand name -> Bool) -> name -> Hand name -> Int
pointsWhen score matches cardName hand = if matches cardName hand then score else 0

pointsForEachCardThat :: Int -> (Card name -> Bool) -> name -> Hand name -> Int
pointsForEachCardThat score matches _ hand = score * Map.size (Map.filter matches hand)

pointsForEachOtherCardThat :: (Eq name) => Int -> (Card name -> Bool) -> name -> Hand name -> Int
pointsForEachOtherCardThat score matches cardName hand =
  score * Map.size (Map.filterWithKey otherCardMatches hand)
  where
    otherCardMatches self card = self /= cardName && matches card

hasSuit :: Suit -> Predicate (Card name)
hasSuit wantedSuit card = suit card == wantedSuit

hasOneOfSuits :: [Suit] -> Predicate (Card name)
hasOneOfSuits wantedSuits card = suit card `elem` wantedSuits

hasName :: (Eq name) => name -> Predicate (Card name)
hasName cardName card = cardName == name card

(<+>) :: (a -> b -> Int) -> (a -> b -> Int) -> a -> b -> Int
(<+>) = liftA2 (liftA2 (+))

infixr 6 <+>

-- | Expands the given cards to a hand.
--
-- All cards have base properties, and still need to have their effects applied to each other.
initializeHand :: (Eq name, FantasyRealms name) => Set name -> Hand name
initializeHand cardNames =
  Map.fromAscList
    [ (cardName, describeCard cardName)
      | cardName <- Set.toAscList cardNames
    ]

-- | Compute the various effects that cards have on each other, together with
-- whether a card is blanked.
computeEffects :: Hand name -> Map name (Card name, Bool)
computeEffects = blankCard . clearPenalties

blankCard :: Hand name -> Map name (Card name, Bool)
blankCard hand = Map.map updateCard hand
  where
    updateCard card = (card, any (`penaltyBlanks` card) hand)

clearPenalties :: forall name. Hand name -> Hand name
clearPenalties hand = Map.map updateCard hand
  where
    updateCard :: Card name -> Card name
    updateCard card
      | isPenaltyCleared card = clearPenalty card
      | otherwise = card

    isPenaltyCleared :: Card name -> Bool
    isPenaltyCleared = foldr ((||*) . clearsPenalty) false (Map.elems hand)

    clearPenalty :: Card name -> Card name
    clearPenalty card =
      card
        { penaltyScore = \_ _ -> 0,
          penaltyBlanks = const False
        }

-- | Score a hand of cards based on their current properties.
--
-- Each card is mapped to its individual score, if it is not blanked.
scoreHand :: forall name. Map name (Card name, Bool) -> Map name (Maybe Int)
scoreHand handWithBlanks = Map.mapWithKey scoreCard handWithBlanks
  where
    handWithoutBlanks :: Hand name
    handWithoutBlanks = fmap fst (Map.filter (not . snd) handWithBlanks)

    scoreCard :: name -> (Card name, Bool) -> Maybe Int
    scoreCard cardName (card, blanked)
      | blanked = Nothing
      | otherwise =
          Just $
            baseStrength card
              + bonusScore card cardName handWithoutBlanks
              + penaltyScore card cardName handWithoutBlanks
