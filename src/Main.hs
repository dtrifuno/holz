module Main where

import Control.Monad (forever)
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
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
  bottomStr <- T.IO.getLine
  putStr "Your Middle row: "
  middleStr <- T.IO.getLine
  putStr "Your Top row: "
  topStr <- getLine
  putStr "Opponent's Top row: "
  oppTopStr <- T.IO.getLine
  putStr "Opponent's Middle row: "
  oppMiddleStr <- T.IO.getLine
  putStr "Opponent's Top row: "
  oppTopStr <- T.IO.getLine
  putStr "Card to play: "
  cardStr <- T.IO.getLine
  return ()
