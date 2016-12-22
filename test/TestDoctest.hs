{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.DocTest

main :: IO ()
main = doctest ["-isrc", "src/Cards.hs",
                         "src/Evaluate.hs",
                         "src/Main.hs",
                         "src/Simulate.hs",
                         "src/Util.hs"]
