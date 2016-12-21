{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Maybe
import System.Exit
import Test.HUnit

import Cards
import Evaluate

test_both_foul = TestCase (do let p1 = toArrayPlayer
                                    (Player
                                        (fromJust $ parseCards "Ah,Ac,Js,Jd,5s")
                                        (fromJust $ parseCards "6d,8d,9d,Td,Qd")
                                        (fromJust $ parseCards "Ks,Kd,4c"))
                              let p2 = toArrayPlayer
                                    (Player
                                        (fromJust $ parseCards "6h,6c,6s,3d,3s")
                                        (fromJust $ parseCards "8s,8h,Ts,Th,5d")
                                        (fromJust $ parseCards "Qh,Qs,Qc"))
                              assertEqual "Is score 0?" 0 (scoreGame p1 p2))
tests = TestList [TestLabel "test_both_foul" test_both_foul]

main :: IO ()
main = do
  cs@(Counts _ _ errs fails) <- runTestTT tests
  if errs > 0 || fails > 0
    then exitFailure
    else exitSuccess
