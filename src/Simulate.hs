module Simulate where

import Control.Applicative ((<*>))
import Control.Monad (foldM_)
import Data.List ((\\))
import Data.IORef

import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

import Cards
import Evaluate
import Util (shuffleTo)

-- |Player datatype where rows are encoded as mutable arrays for fast fills.
-- bEmpty (resp. mEmpty and tEmpty) are the number of slots in the array to
-- fill during a run.
data Player'' = Player''
  { bottom'' :: UM.IOVector Card'
  , middle'' :: UM.IOVector Card'
  , top''    :: UM.IOVector Card'
  , bEmpty   :: Int
  , mEmpty   :: Int
  , tEmpty   :: Int
  }

simulate :: Player -> Player -> Int -> IO Double
simulate p1 p2 n = do
  deck'' <- U.thaw (U.fromList $ map cardToWord32 (remainingCards p1 p2))
  p1'' <- preparePlayer p1
  p2'' <- preparePlayer p2
  val <- newIORef (0 :: Int)
  simulateNTimes p1'' p2'' deck'' val n
  sumRating <- readIORef val
  return (fromIntegral sumRating / fromIntegral n :: Double)

-- |Returns a list of all cards not currently held by a player.
remainingCards :: Player -> Player -> [Card]
remainingCards (Player b1 m1 t1) (Player b2 m2 t2) =
  deck \\ (b1 ++ m1 ++ t1 ++ b2 ++ m2 ++ t2)

-- FIXME: Very ugly.
preparePlayer :: Player -> IO Player''
preparePlayer (Player b m t) = do
  b'' <- U.thaw (U.fromList $ map cardToWord32 b)
  let bEmpty' = 5 - UM.length b''
  b' <- if bEmpty' > 0 then UM.grow b'' bEmpty' else return b''
  m'' <- U.thaw (U.fromList $ map cardToWord32 m)
  let mEmpty' = 5 - UM.length m''
  m' <- if mEmpty' > 0 then UM.grow m'' mEmpty' else return m''
  t'' <- U.thaw (U.fromList $ map cardToWord32 t)
  let tEmpty' = 3 - UM.length t''
  t' <- if tEmpty' > 0 then UM.grow t'' tEmpty' else return t''
  return (Player'' b' m' t' bEmpty' mEmpty' tEmpty')

thawPlayer :: Player' -> Int -> Int -> Int -> IO Player''
thawPlayer (Player' b m t) bEmpty mEmpty tEmpty = do
  b' <- U.unsafeThaw b
  m' <- U.unsafeThaw m
  t' <- U.unsafeThaw t
  return (Player'' b' m' t' bEmpty mEmpty tEmpty)

freezePlayer :: Player'' -> IO Player'
freezePlayer (Player'' b m t _ _ _) = do
  b' <- U.unsafeFreeze b
  m' <- U.unsafeFreeze m
  t' <- U.unsafeFreeze t
  return (Player' b' m' t')

fill :: Player'' -> Player'' -> UM.IOVector Card' -> IO ()
fill p1 p2 deck'' = foldM_ (\x y -> fillRow y deck'' x) 0 [
    (Bottom, bottom'' p1, 5 - bEmpty p1),
    (Middle, middle'' p1, 5 - mEmpty p1),
    (Top, top'' p1, 3 - tEmpty p1),
    (Bottom, bottom'' p2, 5 - bEmpty p2),
    (Middle, middle'' p2, 5 - mEmpty p2),
    (Top, top'' p2, 3 - tEmpty p2)]

fillRow :: (RowType, UM.IOVector Card', Int) -> UM.IOVector Card' -> Int -> IO Int
fillRow (Bottom, _, 5) _ i = return i
fillRow (Middle, _, 5) _ i = return i
fillRow (Top, _, 3) _ i = return i
fillRow (rowType, row, j) deck i = do
  card <- UM.read deck i
  UM.write row j card
  fillRow (rowType, row, j+1) deck (i+1)

simulateNTimes :: Player'' -> Player'' -> UM.IOVector Card' ->
                  IORef Int-> Int -> IO ()
simulateNTimes _ _ _ _ 0        = return ()
simulateNTimes p1 p2 deck val n = do
  (p1', p2') <- simulateOnce p1 p2 deck val
  simulateNTimes p1' p2' deck val (n-1)

simulateOnce :: Player'' -> Player'' -> UM.IOVector Card' ->
                IORef Int-> IO (Player'', Player'')
simulateOnce p1 p2 deck val = do
  shuffleTo howMany deck
  fill p1 p2 deck
  frozenP1 <- freezePlayer p1
  frozenP2 <- freezePlayer p2
  let rate = fromIntegral $ rateHands frozenP1 frozenP2 :: Int
  modifyIORef' val (+rate)
  p1' <- thawPlayer frozenP1 (bEmpty p1) (mEmpty p1) (tEmpty p1)
  p2' <- thawPlayer frozenP2 (bEmpty p2) (mEmpty p2) (tEmpty p2)
  return (p1', p2')
  where howMany = sum ([bEmpty, mEmpty, tEmpty] <*> [p1, p2])
  --putStrLn "DEBUG: Finished simulating once"
