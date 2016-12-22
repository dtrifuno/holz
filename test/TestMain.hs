{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Maybe
import qualified Data.Text as T
import System.Exit
import Test.HUnit

import Cards
import Evaluate

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x, y, z) = f x y z

map3 :: (a -> b) -> (a, a, a) -> (b, b, b)
map3 f (x, y, z) = (f x, f y, f z)

bothFoulData = (("Ah,Ac,Js,Jd,5s", "6d,8d,9d,Td,Qd", "Ks,Kd,4c"),
                ("6h,6c,6s,3d,3s", "8s,8h,Ts,Th,5d", "Qh,Qs,Qc"),
                "",
                0)

oneFoulData = (("2h,2d,Js,Jd,7d", "4c,4h,5s,9s,2c", "Ah,Ks,6s"),
               ("Td,9h,Kh,Qd,Tc", "7c,8h,Th,6c,9c", "2s,5h,Ad"),
               "",
               6)

normal1Data = (("6h,7d,8d,9s,Ts", "Jc,Th,Kd,Jh,Jd", "Ad,Ah,3c"),
               ("2s,5s,Js,Qs,As", "4d,2d,6d,3h,5h", "8h,8s,Qd"),
               "",
               1)

normal2Data = (("Kh,Qd,Ah,4d,Qh", "9s,9d,4s,5h,Jh", "3d,5c,As"),
               ("7c,6c,Kc,9c,Qc", "Td,Th,8h,Ts,5s", "7s,Kd,2h"),
               "",
               -7)

scoopData = (("Ts,Th,Jd,Jh,2s", "8h,7c,9d,9s,As", "7h,3s,4s"),
             ("6c,5c,9c,3c,Kc", "Qd,Td,Js,Ks,Qs", "8s,7s,Kh"),
             "",
             -10)

createPlayer' :: (T.Text, T.Text, T.Text) -> Player'
createPlayer' cs = toArrayPlayer $
                    uncurry3 Player (map3 (fromJust . parseCards) cs)

scoringTest (c1, c2, str, n) = TestCase (do let p1 = createPlayer' c1
                                            let p2 = createPlayer' c2
                                            assertEqual str n (scoreGame p1 p2))

tests = TestList [ TestLabel "test_both_foul" (scoringTest bothFoulData)
                 , TestLabel "test_both_foul" (scoringTest oneFoulData)
                 , TestLabel "test_both_foul" (scoringTest normal1Data)
                 , TestLabel "test_both_foul" (scoringTest normal2Data)
                 , TestLabel "test_both_foul" (scoringTest scoopData)
                 ]

main :: IO ()
main = do
  cs@(Counts _ _ errs fails) <- runTestTT tests
  if errs > 0 || fails > 0
    then exitFailure
    else exitSuccess
