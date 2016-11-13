{-# LANGUAGE BinaryLiterals #-}

module Cards where

import Data.Bits
import Data.Word (Word32, Word16)
import Text.Printf

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

type Card32 = Word32

type HandStrength16 = Word16

-- | Encode card as an Word32 for Cactus Kev's hand evaluation algorithm
--
-- >>> printBits . cardToWord32 $ Card King Diamonds
-- "00001000000000000100101100100101"
-- >>> printBits . cardToWord32 $ Card Five Spades
-- "00000000000010000001001100000111"
--
cardToWord32 :: Card -> Card32
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
toCard :: Card32 -> Card
toCard w = Card (getRank w) (getSuit w)
  where getSuit w = case w .&. 0xf000 of
          0x1000 -> Spades
          0x2000 -> Hearts
          0x4000 -> Diamonds
          0x8000 -> Clubs
        getRank w = (toEnum . fromIntegral $ countTrailingZeros (shift w (-16))) :: Rank

printBits :: Word32 -> String
printBits = printf "%032b"
