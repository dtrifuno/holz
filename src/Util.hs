module Util where

import Data.Array.MArray
import Data.Array.IO

import qualified Data.Text as T

import System.Random

-- | Strips all whitespace from a Text.
--
-- >>> :set -XOverloadedStrings
-- >>> stripWhitespace "\t\tba  na   na     \ns\t  "
-- "bananas"
stripWhitespace :: T.Text -> T.Text
stripWhitespace = T.filter (`notElem` [' ', '\n', '\t'])
