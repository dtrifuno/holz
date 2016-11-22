module Simulate where

import Data.List ((\\))

import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

import Cards
import Evaluate

data Player'' = Player''
  { bottom'' :: UM.IOVector Card'
  , middle'' :: UM.IOVector Card'
  , top''    :: UM.IOVector Card'
  }

simulate :: Player -> Player -> Int -> IO Double
simulate p1 p2 n = do
  deck' <- U.thaw (U.fromList $ map cardToWord32 (remainingCards p1 p2))
  b1 <- thaw' 5 (U.fromList $ map cardToWord32 (bottom p1))
  return 5

remainingCards :: Player -> Player -> [Card]
remainingCards (Player b1 m1 t1) (Player b2 m2 t2) =
  deck \\ (b1 ++ m1 ++ t1 ++ b2 ++ m2 ++ t2)

thaw' :: Int -> U.Vector Card' -> IO (UM.IOVector Card')
thaw' n vec = do
  uvec' <- U.thaw vec
  uvec <- UM.grow uvec' (n - U.length vec)
  return uvec

thawPlayer :: Player' -> IO Player''
thawPlayer (Player' b m t) = do
  b' <- U.thaw b
  m' <- U.thaw m
  t' <- U.thaw t
  return (Player'' b' m' t')

freezePlayer :: Player'' -> IO Player'
freezePlayer (Player'' b m t) = do
  b' <- U.freeze b
  m' <- U.freeze m
  t' <- U.freeze t
  return (Player' b' m' t')

fill :: [(UM.IOVector Card', Int)] -> UM.IOVector Card' -> IO ()
fill xs deck' = fill' xs deck' 1
  where fill' [] _ _                = return ()
        fill' ((vec, i):cs) deck' j = do
          card <- UM.read deck' j
          UM.write vec i card
          fill' cs deck' (j+1)
