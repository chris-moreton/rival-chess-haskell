{-# LANGUAGE OverloadedStrings #-}

module Util.Utils where

import Data.List
import Types
import Data.Bits

substring :: String -> Int -> Int -> String
substring text start end = take (end - start) (drop start text)

fromSquareMask :: Square -> MoveMask
fromSquareMask sq = sq `shiftL` 16

fromSquarePart :: Move -> Square
fromSquarePart move = move `shiftR` 16

toSquarePart :: Move -> Square
toSquarePart move = (.&.) move 63
