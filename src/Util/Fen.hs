{-# LANGUAGE ViewPatterns #-}

module Util.Fen (
     fenRanks
   , fenBoardPart
   , rankBits)
    where

import Data.List.Split
import Data.Char

fenBoardPart :: String -> String
fenBoardPart fen = head (splitOn " " fen)

fenRanks :: String -> [String]
fenRanks = splitOn "/"

isFileNumber :: Char -> Bool
isFileNumber c = ord c >= 49 && ord c <= 56

rankBits :: String -> Char -> [Int]
rankBits x y = addRankBits x y []

addRankBits :: String -> Char -> [Int] -> [Int]
addRankBits [] _ result = result
addRankBits fenRankChars pieceChar result = do
  let c = head fenRankChars
  let thisResult = if (isFileNumber c) then (take ((ord c) - 48) (repeat 0)) else (if (pieceChar == c) then [1] else [0])
  addRankBits (tail fenRankChars) pieceChar (result ++ thisResult)
