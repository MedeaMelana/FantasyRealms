{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Scoring calculator for the card game
-- [Fantasy Realms](https://boardgamegeek.com/boardgame/223040/fantasy-realms).
module FantasyRealms
  ( -- * Types
    FantasyRealms (..),
    Card (..),
    Suit (..),
    Hand,

    -- * Describing cards

    -- | These modifiers can be applied to cards using `Data.Function.(&)`.
    withBonusScore,
    clearingPenalty,
    withPenaltyScore,
    blanking,

    -- * Scoring bonus or penalty points
    pointsWhen,
    hasCardThat,
    pointsForEachCardThat,
    pointsForEachOtherCardThat,

    -- * Predicates on other cards
    hasSuit,
    hasName,
    hasOneOfSuits,

    -- * Scoring hands
    initializeHand,
    applyEffects,
    scoreHand,
  )
where

import Control.Applicative (liftA2)
import Data.Boolean (Boolean (false), (||*))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

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

-- | A card's full properties, including how it affects and is affected by other
-- cards.
--
-- A card is parametrised by its name so that concrete card implementations can
-- live outside this module.
data Card name = Card
  { -- | The card's name.
    name :: name,
    -- | The card's base strength.
    baseStrength :: Int,
    -- | The card's suit.
    suit :: Suit,
    -- | Given the card's own original name and a hand of cards, computes any
    -- bonuses to the score.
    bonusScore :: name -> Hand name -> Int,
    -- | Whether the given card's penalties are cleared by this card.
    bonusClearsPenalty :: Card name -> Bool,
    -- | Given the card's own original name and a hand of cards, computes any
    -- penalties to the score. The resulting score is expected to be a negative
    -- number.
    penaltyScore :: name -> Hand name -> Int,
    -- | Whether the given card is blanked by this card.
    penaltyBlanks :: Card name -> Bool
  }

-- | The class of Fantasy Realms card names.
class FantasyRealms name where
  -- | Given a card name, describe the card's properties.
  describeCard :: name -> Card name

-- | Adds a bonus score to a card.
--
-- Any existing bonus scores are added to the new bonus score.
withBonusScore :: (name -> Hand name -> Int) -> Card name -> Card name
withBonusScore f c@(Card {bonusScore}) = c {bonusScore = bonusScore <+> f}

-- | Adds a new class of cards whose penalties are cleared by this card.
--
-- Any existing penalties cleared are kept and combined with the new predicate.
clearingPenalty :: (Card name -> Bool) -> Card name -> Card name
clearingPenalty f c@(Card {bonusClearsPenalty}) = c {bonusClearsPenalty = bonusClearsPenalty ||* f}

-- | Adds a penalty score to this card.
--
-- Any existing penalty scores are added to the new penalty score.
withPenaltyScore :: (name -> Hand name -> Int) -> Card name -> Card name
withPenaltyScore f c@(Card {penaltyScore}) = c {penaltyScore = penaltyScore <+> f}

-- | Adds a new class of cards that are blanked by this card.
--
-- Any existing blanked cards are kept and combined with the new predicate.
blanking :: (Card name -> Bool) -> Card name -> Card name
blanking f c@(Card {penaltyBlanks}) = c {penaltyBlanks = penaltyBlanks ||* f}

(<+>) :: (a -> b -> Int) -> (a -> b -> Int) -> a -> b -> Int
(<+>) = liftA2 (liftA2 (+))

infixr 6 <+>

-- | Scores points when a condition is met.
pointsWhen :: Int -> (name -> Hand name -> Bool) -> name -> Hand name -> Int
pointsWhen score matches cardName hand = if matches cardName hand then score else 0

-- | Whether at least one of a hand of cards meets the given condition.
hasCardThat :: (Card name -> Bool) -> name -> Hand name -> Bool
hasCardThat matches _ = any matches

-- | Scores points for each card that meets the given condition.
pointsForEachCardThat :: Int -> (Card name -> Bool) -> name -> Hand name -> Int
pointsForEachCardThat score matches _ hand = score * Map.size (Map.filter matches hand)

-- | Scores points for each card other than the current card that meets the
-- given condition.
pointsForEachOtherCardThat :: (Eq name) => Int -> (Card name -> Bool) -> name -> Hand name -> Int
pointsForEachOtherCardThat score matches cardName hand =
  score * Map.size (Map.filterWithKey otherCardMatches hand)
  where
    otherCardMatches self card = self /= cardName && matches card

-- | Whether a card has the given name.
hasName :: (Eq name) => name -> Card name -> Bool
hasName cardName card = cardName == name card

-- | Whether a card has the given suit.
hasSuit :: Suit -> Card name -> Bool
hasSuit wantedSuit card = suit card == wantedSuit

-- | Whether a card has one of given suits.
hasOneOfSuits :: [Suit] -> Card name -> Bool
hasOneOfSuits wantedSuits card = suit card `elem` wantedSuits

-- | Expands the given card names to a hand of cards and their base properties.
--
-- All cards have base properties, and still need to have their effects applied to each other.
initializeHand :: (Eq name, FantasyRealms name) => Set name -> Hand name
initializeHand cardNames =
  Map.fromAscList
    [ (cardName, describeCard cardName)
      | cardName <- Set.toAscList cardNames
    ]

-- | Given a hand of cards, applies the various effects that cards have on each
-- other. The resulting map contains cards with updated properties, as well as a
-- boolean indicating whether the card was blanked.
applyEffects :: Hand name -> Map name (Card name, Bool)
applyEffects = blankCard . clearPenalties

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
    isPenaltyCleared = foldr ((||*) . bonusClearsPenalty) false (Map.elems hand)

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
