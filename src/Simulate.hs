module Simulate where

import Data.List ((\\))

import Cards
import Evaluate

simulate :: Player -> Player -> Int -> Double
simulate _ _ _ = 5.0

remCards :: Player -> Player -> [Card]
remCards (Player b1 m1 t1) (Player b2 m2 t2) =
  deck \\ (b1 ++ m1 ++ t1 ++ b2 ++ m2 ++ t2)
