module Util where

import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.Text as T

import Control.Monad
import System.Random

-- | Strips all whitespace from a Text.
--
-- >>> :set -XOverloadedStrings
-- >>> stripWhitespace "\t\tba  na   na     \ns\t  "
-- "bananas"
stripWhitespace :: T.Text -> T.Text
stripWhitespace = T.filter (`notElem` [' ', '\n', '\t'])

shuffle :: UM.Unbox a => UM.IOVector a -> IO ()
shuffle vec = forM_ [0..(n-2)] $ \i -> do
  j <- randomRIO (i, n - 1)
  UM.swap vec i j
  where n = UM.length vec
