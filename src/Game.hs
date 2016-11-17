module Game where

import Cards
import HandStrength.Data

import Data.Word
import Control.Arrow ((***))

import Data.Bits
import qualified Data.Array.Unboxed as UA
import qualified Data.IntMap.Strict as M

type Row     = UA.Array Int Card32
data RowType = Bottom | Middle | Top
data Player  = Player Row Row Row

flushesArray = UA.listArray (0, length flushesList - 1) (map fromIntegral flushesList) :: UA.Array Int HandStrength16

uniquesArray = UA.listArray (0, length uniquesList - 1) (map fromIntegral uniquesList) :: UA.Array Int HandStrength16

multiplesMap = M.fromList (map (fromIntegral *** fromIntegral) multiplesList :: [(Int, HandStrength16)])

threesMap = M.fromList (map (fromIntegral *** fromIntegral) threesList :: [(Int, HandStrength16)])


royalty :: (Foldable t) => RowType -> t Card32 -> Int
royalty Top xs | hs >= 6241 = 22 --AAA
               | hs >= 6174 = 21 --KKK
               | hs >= 6107 = 20 --QQQ
               | hs >= 6040 = 19 --JJJ
               | hs >= 5973 = 18 --TTT
               | hs >= 5906 = 17 --999
               | hs >= 5839 = 16 --888
               | hs >= 5772 = 15 --777
               | hs >= 5705 = 14 --666
               | hs >= 5638 = 13 --555
               | hs >= 5571 = 12 --222
               | hs >= 5504 = 11 --333
               | hs >= 5437 = 10 --222
               | hs >= 4347 =  9 --AA*
               | hs >= 4115 =  8 --KK*
               | hs >= 3883 =  7 --QQ*
               | hs >= 3651 =  6 --JJ*
               | hs >= 3419 =  5 --TT*
               | hs >= 3187 =  4 --99*
               | hs >= 2955 =  3 --88*
               | hs >= 2723 =  2 --77*
               | hs >= 2491 =  1 --66*
               | otherwise  =  0
               where hs = evalHand xs
royalty Middle xs | hs >= 7916 = 50 --Royal Flush
                  | hs >= 7907 = 30 --Straight Flush
                  | hs >= 7751 = 20 --Quads
                  | hs >= 7595 = 12 --Full House
                  | hs >= 6318 =  8 --Flush
                  | hs >= 6309 =  4 --Straight
                  | hs >= 5438 =  2 --Set
                  | otherwise  =  0
                  where hs = evalHand xs
royalty Bottom xs | hs >= 7916 = 25 --Royal Flush
                  | hs >= 7907 = 15 --Straight Flush
                  | hs >= 7751 = 10 --Quads
                  | hs >= 7595 =  6 --Full House
                  | hs >= 6318 =  4 --Flush
                  | hs >= 6309 =  2 --Straight
                  | otherwise  = 0
                  where hs = evalHand xs

royalties :: Player -> Int
royalties (Player b1 m1 t1) = royalty Bottom b1 - royalty Middle m1 - royalty Top t1

rateHands :: Player -> Player -> Int
rateHands p1@(Player b1 m1 t1) p2@(Player b2 m2 t2) = royalties p1 - royalties p2
 + if evalHand b1 > evalHand b2 then 1 else 0
 + if evalHand b2 > evalHand b1 then (-1) else 0
 + if evalHand m1 > evalHand m2 then 1 else 0
 + if evalHand m2 > evalHand m1 then (-1) else 0
 + if evalHand t1 > evalHand t2 then 1 else 0
 + if evalHand t2 > evalHand t1 then (-1) else 0

-- Has p1 scooped p2?
scooped :: Player -> Player -> Bool
scooped (Player b1 m1 t1) (Player b2 m2 t2) = (evalHand b1 > evalHand b2)
    && (evalHand m1 > evalHand t2)
    && (evalHand t1 > evalHand t2)

fouled :: Player -> Bool
fouled (Player b1 m1 t1) = (evalHand t1 <= evalHand m1) && (evalHand m1 <= evalHand b1)

scoreGame :: Player -> Player -> Int
scoreGame p1@(Player b1 m1 t1) p2@(Player b2 m2 t2)
  | fouled p1 && fouled p2 = 0
  | fouled p1              = 6 + royalties p2
  | fouled p2              = -6 - royalties p1
  | scooped p1 p2          = 3 + rateHands p1 p2
  | scooped p2 p1          = -3 + rateHands p1 p2
  | otherwise              = rateHands p1 p2


evalHand :: (Foldable t) => t Card32 -> HandStrength16
evalHand xs | length xs == 3 = evalHandThree xs
            | length xs == 5 = evalHandFive xs

isFlush :: (Foldable t) => t Card32 -> Bool
isFlush xs = foldr (.&.) 0xf000 xs /= 0

evalFlush :: (Foldable t) => t Card32 -> Word32
evalFlush xs = shift (foldr (.|.) 0 xs) (-16)

evalMultiples :: (Foldable t) => t Card32 -> Word32
evalMultiples = foldr (\x y -> (x .&. 0xff) * y) 1

evalHandFive :: (Foldable t) => t Card32 -> HandStrength16
evalHandFive xs | isFlush xs = flushesArray UA.! fromIntegral (evalFlush xs)
                | uniquesArray UA.! fromIntegral (evalFlush xs) /= 0 = uniquesArray UA.! fromIntegral (evalFlush xs)
                | otherwise = multiplesMap M.! fromIntegral (evalMultiples xs)

evalHandThree :: (Foldable t) => t Card32 -> HandStrength16
evalHandThree xs = threesMap M.! fromIntegral (evalMultiples xs)
