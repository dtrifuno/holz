{-# LANGUAGE OverloadedStrings #-}

module Main where


import Control.Monad (forever)
import Data.List (nub)
import qualified Data.Text.IO as T.IO
import System.IO

import Cards
import Simulate

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  forever $ do
    handleSetting
    putStrLn ""

handleSetting :: IO ()
handleSetting = do
  putStr "Input cards as a comma-separated list in shorthand notation."
  putStrLn " (example: Ah,Th,5h)\n"
  bottom' <- promptCards "Your Bottom row: "
  middle' <- promptCards "Your Middle row: "
  top' <- promptCards "Your Top row: "
  oppBottom <- promptCards "Opponent's Bottom row: "
  oppMiddle <- promptCards "Opponent's Middle row: "
  oppTop <- promptCards "Opponent's Top row: "
  card <- promptCard "Card you are about to play: "
  let p1 = Player bottom' middle' top'
  let p2 = Player oppBottom oppMiddle oppTop
  if validate p1 p2 card
    then giveEVs p1 p2 card
    else putStrLn "Not a valid setting. Did you reuse a card? Try again."
  return ()

promptCards :: String -> IO [Card]
promptCards txt = do
  putStr txt
  gotTxt <- T.IO.getLine
  let cards = parseCards gotTxt
  maybe (do
    putStrLn "Not a valid list of cards. Try again."
    promptCards txt)
    return cards

promptCard :: String -> IO Card
promptCard txt = do
  putStr txt
  gotTxt <- T.IO.getLine
  maybe (do
   putStrLn "Not a valid card. Try again."
   promptCard txt)
   return (parseCard gotTxt)

validate :: Player -> Player -> Card -> Bool
validate (Player b1 m1 t1) (Player b2 m2 t2) c =
  cards == nub cards
  where cards = c:(b1 ++ m1 ++ t1 ++ b2 ++ m2 ++ t2)

sims :: Int
sims = 1000

giveEVs :: Player -> Player -> Card -> IO ()
giveEVs (Player b1 m1 t1) p2 c = do
  putStrLn ""
  if length t1 < 3
    then do res <- (simulate (Player b1 m1 t1') p2 sims)
            printResult "Top:    " res
    else return ()
  if length m1 < 5
    then do res <- (simulate (Player b1 m1' t1) p2 sims)
            printResult "Middle: " res
    else return ()
  if length b1 < 5
    then do res <- (simulate (Player b1' m1 t1) p2 sims)
            printResult "Bottom: " res
    else return ()
  where printResult str f = putStrLn $ str ++ show f
        b1' = c:b1
        m1' = c:m1
        t1' = c:t1
