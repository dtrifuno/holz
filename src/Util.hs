module Util where

import Data.Array.MArray
import Data.Array.IO

import qualified Data.Text as T

import System.Random

stripWhitespace :: T.Text -> T.Text
stripWhitespace = T.filter (\x -> not $ elem x [' ', '\n', '\t'])
