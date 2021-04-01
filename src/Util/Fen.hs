{-# LANGUAGE ViewPatterns #-}

module Util.Fen (
     fenRanks
   , fenBoardPart
   , rankBits
   , boardBits
   , pieceBitboard
   , algebraicSquareRefFromBitRef
   , bitRefFromAlgebraicSquareRef)
    where

import Data.List.Split
import Data.Char
import Data.Bits

fenBoardPart :: String -> String
fenBoardPart fen = head (splitOn " " fen)

fenRanks :: String -> [String]
fenRanks = splitOn "/"

isFileNumber :: Char -> Bool
isFileNumber c = ord c >= 49 && ord c <= 56

rankBits :: String -> Char -> [Int]
rankBits x y = recurRankBits x y []

recurRankBits :: String -> Char -> [Int] -> [Int]
recurRankBits [] _ result = result
recurRankBits fenRankChars pieceChar result = do
  let c = head fenRankChars
  let thisResult = if isFileNumber c then take (ord c - 48) (repeat 0) else (if pieceChar == c then [1] else [0])
  recurRankBits (tail fenRankChars) pieceChar (result ++ thisResult)

boardBits :: [String] -> Char -> [Int]
boardBits x y = recurBoardBits x y []

recurBoardBits :: [String] -> Char -> [Int] -> [Int]
recurBoardBits [] _ result = result
recurBoardBits fenRanks pieceChar result = do
  let thisResult = rankBits (head fenRanks) pieceChar
  recurBoardBits (tail fenRanks) pieceChar (result ++ thisResult)

bitArrayToDecimal :: [Int] -> Int
bitArrayToDecimal bits = recurBitArrayToDecimal bits 63 0

recurBitArrayToDecimal :: [Int] -> Int -> Int -> Int
recurBitArrayToDecimal _ (-1) result = result
recurBitArrayToDecimal bits bitnum result = do
  let thisResult = if head bits == 1 then shiftL 1 bitnum else 0
  recurBitArrayToDecimal (tail bits) (bitnum - 1) (result + thisResult)

pieceBitboard :: [String] -> Char -> Int
pieceBitboard fenRanks pieceChar = bitArrayToDecimal (boardBits fenRanks pieceChar)

algebraicSquareRefFromBitRef :: Int -> String
algebraicSquareRefFromBitRef bitRef = do
  let rank = quot bitRef 8 + 1
  let file = 8 - mod bitRef 8
  let rankChar = chr (rank + 48)
  let fileChar = chr (file + 96)
  [fileChar,rankChar]

bitRefFromAlgebraicSquareRef :: String -> Int
bitRefFromAlgebraicSquareRef algebraic = do
  let fileNum = ord (head algebraic) - 97
  let rankNum = ord (head (tail algebraic)) - 49
  (rankNum * 8) + (7 - fileNum)
