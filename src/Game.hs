module Game where

import Hands (Card32)

import Data.Bits

type HandStrength = Word32

evalHand :: [Card32] -> HandStrength
evalHand xs | length xs == 3 = evalHandThree xs
            | length xs == 5 = evalHandFive


isFlush :: [Card32] -> Bool
isFlush xs = (foldr (.&.) 0xf000 xs) /= 0

evalFlush :: [Card32] -> Word32
evalFlush xs = shift (foldr (.|.) 0 xs) (-16)

evalMultiples :: [Card32] -> Word32
evalMultiples = foldr (\x y -> (x & 0xff) * y) 1 

evalHandFive :: [Card32] -> HandStrength
evalHandFive xs | isFlush xs = error 'q'
