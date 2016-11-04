{-# LANGUAGE BinaryLiterals #-}

module Hands where

import Data.Bits
import Data.Word (Word32)
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


-- | Encode card as an Word32 for Cactus Kev's hand evaluation algorithm
--
-- >>> printBits . cardToWord32 $ Card King Diamonds
-- "00001000000000000100101100100101"
-- >>> printBits . cardToWord32 $ Card Five Spades
-- "00000000000010000001001100000111"
--
cardToWord32 :: Card -> Word32
cardToWord32 (Card r s) =
    primeRank r
  + shift (fromIntegral $ fromEnum r) 8
  + shift 1 (12 + fromEnum s)
  + shift 1 (16 + fromEnum r)

--flushes :: [Card] -> 

--toCard :: Int32 -> Card
--toCard


printBits :: Word32 -> String
printBits = printf "%032b"
