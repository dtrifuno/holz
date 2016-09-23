module Hands where

import Data.Bits

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

primeRank :: Rank -> Int
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

cardToInt :: Card -> Int
cardToInt (Card r s) =
    primeRank r
  + shift (fromEnum r) 8
  + shift 1 (12 + fromEnum s)
  + shift 1 (16 + fromEnum r)

printBits :: Int -> String
printBits s = reverse (printBit s 0)

printBit :: Int -> Int -> String
printBit s 31 = if s .&. bit 31 /= 0 then "1" else "0"
printBit s n  = if s .&. bit n /= 0
                then '1':printBit s (n+1)
                else '0':printBit s (n+1)
