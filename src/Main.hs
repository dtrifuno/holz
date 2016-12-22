{-# LANGUAGE OverloadedStrings #-}

module Main where


import Control.Monad (forever, when)
import Data.List (nub)
import qualified Data.Text.IO as T.IO
import System.IO

import Prompt

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  forever $ do
    handleSetting
    putStrLn ""
