module Util where

import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.Text as T

import System.Random

-- | Strips all whitespace from a Text.
--
-- >>> :set -XOverloadedStrings
-- >>> stripWhitespace "\t\tba  na   na     \ns\t  "
-- "bananas"
stripWhitespace :: T.Text -> T.Text
stripWhitespace = T.filter (`notElem` [' ', '\n', '\t'])

shuffle :: UM.Unbox a => UM.IOVector a -> IO ()
shuffle vec = shuffle' (UM.length vec - 1) vec
  where shuffle' 0 _   = return ()
        shuffle' i vec = do
          j <- randomRIO (0, i)
          UM.swap vec i j
          shuffle' (i-1) vec
