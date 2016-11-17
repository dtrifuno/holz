import Data.Array.MArray
import Data.Array.IO

import System.Random

testArray = newListArray (1, 5) [2, 5, 7, 3, 10]

--shuffle :: MArray Int e
shuffle a = do
  let n = length a
  x <- sequence [randomRIO (0, x) | x <- [n,n-1..1]]
  print x
