{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Maybe (fromJust)

import Prompt (giveEVs)
import Cards

main = giveEVs p1 p2 card
  where b1 = (fromJust . parseCards) "6h,7d,9s,Ts"
        m1 = (fromJust . parseCards) "Jc,Kd,Jd"
        t1 = (fromJust . parseCards) "3c"
        b2 = (fromJust . parseCards) "2s,5s,As"
        m2 = (fromJust . parseCards) "4d,3h,5h"
        t2 = (fromJust . parseCards) "8h,Qd"
        card = (fromJust . parseCard) "Ad"
        p1 = Player b1 m1 t1
        p2 = Player b2 m2 t2
