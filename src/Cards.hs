{-# LANGUAGE BinaryLiterals, OverloadedStrings #-}

module Cards where

import Data.Bits
import Data.Char (ord)
import Data.Maybe (fromJust, isNothing)
import qualified Data.Text as T
import Data.Word (Word32, Word16)
import Text.Printf

import Util

data Rank = Two
          | Three
          | Four
          | Five
          | Six
          | Seven
          | Eight
          | Nine
          | Ten
          | Jack
          | Queen
          | King
          | Ace
          deriving (Eq, Show, Enum)

primeRank :: Rank -> Word32
primeRank c = case c of
  Two   -> 2
  Three -> 3
  Four  -> 5
  Five  -> 7
  Six   -> 11
  Seven -> 13
  Eight -> 17
  Nine  -> 19
  Ten   -> 23
  Jack  -> 29
  Queen -> 31
  King  -> 37
  Ace   -> 41

data Suit = Spades
          | Hearts
          | Diamonds
          | Clubs
          deriving (Eq, Show, Enum)

data Card = Card Rank Suit
          deriving (Eq, Show)

data Player = Player
  {
    bottom :: [Card]
  , middle :: [Card]
  , top    :: [Card]
  }

type Card' = Word32

type HandStrength16 = Word16

-- | Encode card as an Word32 for Cactus Kev's hand evaluation algorithm
--
-- >>> printBits . cardToWord32 $ Card King Diamonds
-- "00001000000000000100101100100101"
-- >>> printBits . cardToWord32 $ Card Five Spades
-- "00000000000010000001001100000111"
--
cardToWord32 :: Card -> Card'
cardToWord32 (Card r s) =
  primeRank r
    + shift (fromIntegral $ fromEnum r) 8
    + shift 1 (12 + fromEnum s)
    + shift 1 (16 + fromEnum r)

-- | Retrieve a card from its Word32 encoding
--
-- >>> toCard 0x81307
-- Card Five Spades
-- >>> toCard 0x200891d
-- Card Jack Clubs
toCard :: Card' -> Card
toCard w = Card getRank getSuit
  where getSuit = case w .&. 0xf000 of
          0x1000 -> Spades
          0x2000 -> Hearts
          0x4000 -> Diamonds
          0x8000 -> Clubs
          _      -> error "Not a valid Word32 encoding of a card."
        getRank = (toEnum . fromIntegral $ countTrailingZeros (shift w (-16))) :: Rank

-- |Parses a card from shorthand notation.
--
-- >>> :set -XOverloadedStrings
-- >>> parseCard "AH"
-- Just (Card Ace Hearts)
-- >>> parseCard "5D"
-- Just (Card Five Diamonds)
parseCard :: T.Text -> Maybe Card
parseCard txt | isNothing (parseRank r) = Nothing
              | isNothing (parseSuit s) = Nothing
              | otherwise = Just (Card (fromJust $ parseRank r)
                                       (fromJust $ parseSuit s))
  where r = T.head (T.toUpper txt)
        s = T.last (T.toUpper txt)
        parseSuit 'S' = Just Spades
        parseSuit 'H' = Just Hearts
        parseSuit 'D' = Just Diamonds
        parseSuit 'C' = Just Clubs
        parseSuit _   = Nothing
        parseRank 'A' = Just Ace
        parseRank 'K' = Just King
        parseRank 'Q' = Just Queen
        parseRank 'T' = Just Ten
        parseRank w   | ord w < ord '2' = Nothing
                      | ord w > ord '9' = Nothing
                      | otherwise = Just (toEnum (ord w - 50) :: Rank)

-- |Parses a comma-separated list of cards in shorthand notation.
--
-- >>> parseCards "Ah,Th,5c"
-- Just [Card Ace Hearts,Card Ten Hearts,Card Five Clubs]
-- >>> parseCards "Ah,TTer,5c"
-- Nothing
-- >>> parseCards ""
-- Just []
parseCards :: T.Text -> Maybe [Card]
parseCards "" = Just []
parseCards cs = mapM parseCard $ T.splitOn "," (stripWhitespace cs)

printBits :: Word32 -> String
printBits = printf "%032b"
