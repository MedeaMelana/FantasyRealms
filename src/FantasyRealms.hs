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

    -- | These modifiers can be applied to cards using 'Data.Function.&'.
    withBonusScore,
    clearingPenalty,
    withPenaltyScore,
    blankingEachCardThat,
    blankingEachOtherCardThat,
    blankedWhen,
    blankedUnless,

    -- * Scoring bonus or penalty points
    pointsWhen,
    hasCardThat,
    pointsForEachCardThat,
    pointsForEachOtherCardThat,

    -- * Predicates on other cards
    hasName,
    hasOneOfNames,
    hasSuit,
    hasOneOfSuits,

    -- * Scoring hands
    initializeHand,
    applyEffects,
    scoreHand,
  )
where

import Control.Applicative (liftA2)
import Data.Boolean (Boolean (notB), (||*))
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
    -- | Whether this card clears the given card's penalties.
    bonusClearsPenalty :: Card name -> Bool,
    -- | Given the card's own original name and a hand of cards, computes any
    -- penalties to the score. The resulting score is expected to be a negative
    -- number.
    penaltyScore :: name -> Hand name -> Int,
    -- | Given the card's own original name and a hand of cards, returns which
    -- cards this card blanks.
    penaltyBlanks :: name -> Hand name -> Set name
  }

-- | Types that represent Fantasy Realms card names.
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

withPenaltyBlanks :: (Ord name) => (name -> Hand name -> Set name) -> Card name -> Card name
withPenaltyBlanks f card = card {penaltyBlanks = penaltyBlanks card <> f}

-- | Adds a new class of cards that are blanked by this card, regardless of
-- whether the considered card is another card or this card itself.
--
-- Any existing blanked cards are kept and combined with the new predicate.
blankingEachCardThat :: (Ord name) => (Card name -> Bool) -> Card name -> Card name
blankingEachCardThat f = withPenaltyBlanks (\_self -> Map.keysSet . Map.filter f)

-- | Adds a new class of cards that are blanked by this card, provided that the
-- considered card is another card.
--
-- Any existing blanked cards are kept and combined with the new predicate.
blankingEachOtherCardThat :: (Ord name) => (Show name) => (Card name -> Bool) -> Card name -> Card name
blankingEachOtherCardThat f = withPenaltyBlanks $ \self hand ->
  let checkCard otherName otherCard = self /= otherName && f otherCard
   in Map.keysSet (Map.filterWithKey checkCard hand)

-- | Markes this card as blanked if the given condition is met.
blankedWhen :: (Ord name) => (name -> Hand name -> Bool) -> Card name -> Card name
blankedWhen f = withPenaltyBlanks $ \self hand ->
  if f self hand then Set.singleton self else Set.empty

-- | Markes this card as blanked if the given condition is not met.
blankedUnless :: (Ord name) => (name -> Hand name -> Bool) -> Card name -> Card name
blankedUnless f = blankedWhen (notB f)

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

-- | Whether a card has one of the given names.
hasOneOfNames :: (Eq name) => [name] -> Card name -> Bool
hasOneOfNames cardNames card = name card `elem` cardNames

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
applyEffects :: (Ord name) => Hand name -> Map name (Card name, Bool)
applyEffects = blankCards . clearPenalties

blankCards :: forall name. (Ord name) => Hand name -> Map name (Card name, Bool)
blankCards hand = Map.mapWithKey checkCard hand
  where
    checkCard :: name -> Card name -> (Card name, Bool)
    checkCard cardName card = (card, Set.member cardName combinedNames)

    combinedNames :: Set name
    combinedNames = Set.unions (Map.mapWithKey perCardNames hand)

    perCardNames :: name -> Card name -> Set name
    perCardNames cardName1 card1 = penaltyBlanks card1 cardName1 hand

clearPenalties :: forall name. Hand name -> Hand name
clearPenalties hand = Map.map updateCard hand
  where
    updateCard :: Card name -> Card name
    updateCard card
      | isPenaltyCleared card = clearPenalty card
      | otherwise = card

    isPenaltyCleared :: Card name -> Bool
    isPenaltyCleared card = any (`bonusClearsPenalty` card) hand

    clearPenalty :: Card name -> Card name
    clearPenalty card =
      card
        { penaltyScore = \_ _ -> 0,
          penaltyBlanks = \_ _ -> Set.empty
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
