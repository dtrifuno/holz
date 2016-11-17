module Main where

import Control.Monad (forever)
import System.IO

import Cards

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  forever $ do
    getSetting
    putStrLn "boo"


getSetting :: IO ()
getSetting = do
  putStrLn "Input cards as comma-separated lists in shorthand notation."
  putStrLn "(example: Ah,Th,5h)"
  putStr "Your Bottom row: "
  bottomStr <- getLine
  putStr "Your Middle row: "
  middleStr <- getLine
  putStr "Your Top row: "
  topStr <- getLine
  putStr "Opponent's Top row: "
  oppTopStr <- getLine
  putStr "Opponent's Middle row: "
  oppMiddleStr <- getLine
  putStr "Opponent's Top row: "
  oppTopStr <- getLine
  putStr "Card to play: "
  cardStr <- getLine
  return ()
