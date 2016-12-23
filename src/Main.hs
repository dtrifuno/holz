{-# LANGUAGE OverloadedStrings #-}

module Main where


import Control.Monad (forever)
import System.IO

import Prompt

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  forever $ do
    handleSetting
    putStrLn ""
