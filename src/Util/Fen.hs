{-# LANGUAGE ViewPatterns, OverloadedStrings #-}

module Util.Fen where

import Types
import Search.MoveConstants

import Data.List.Split
import Data.Char
import Data.Bits
import qualified Data.Text as T

import Util.Utils

startPosition :: String
startPosition = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

fenPart :: String -> Int -> String
fenPart fen index = splitOn " " fen !! index
  
fenBoardPart :: String -> String
fenBoardPart fen = head (splitOn " " fen)

getFenRanks :: String -> [String]
getFenRanks = splitOn "/"

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

pieceBitboard :: [String] -> Char -> Bitboard
pieceBitboard fenRanks pieceChar = fromIntegral(bitArrayToDecimal (boardBits fenRanks pieceChar)) :: Bitboard

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

promotionPart :: Move -> String
promotionPart move
    | (.&.) promotionFullMoveMask move == promotionQueenMoveMask = "q"
    | (.&.) promotionFullMoveMask move == promotionRookMoveMask = "r"
    | (.&.) promotionFullMoveMask move == promotionBishopMoveMask = "b"
    | (.&.) promotionFullMoveMask move == promotionKnightMoveMask = "n"
    | otherwise = ""

promotionMask :: Char -> Int
promotionMask pieceChar
  | pieceChar == 'q' = promotionQueenMoveMask
  | pieceChar == 'b' = promotionBishopMoveMask
  | pieceChar == 'r' = promotionRookMoveMask
  | pieceChar == 'n' = promotionKnightMoveMask
  | otherwise = 0

algebraicMoveFromMove :: Move -> String
algebraicMoveFromMove move = do
  let fromSquare = shiftR move 16
  let toSquare = (.&.) 63 move
  algebraicSquareRefFromBitRef fromSquare ++ algebraicSquareRefFromBitRef toSquare ++ promotionPart move

moveFromAlgebraicMove :: String -> Move
moveFromAlgebraicMove moveString =
  fromSquareMask (bitRefFromAlgebraicSquareRef (substring moveString 0 2)) + bitRefFromAlgebraicSquareRef (substring moveString 2 4) + promotionMask (last moveString)

getMover :: String -> Mover
getMover fen = if fenPart fen 1 == "w" then White else Black
    
enpassantFenPart :: String -> String
enpassantFenPart fen = fenPart fen 3

enPassantBitRef :: String -> Int
enPassantBitRef enPassantFenPart =
  if enPassantFenPart == "-" then enPassantNotAvailable else bitRefFromAlgebraicSquareRef enPassantFenPart

getCastlePrivs :: String -> CastlePrivileges
getCastlePrivs fen =
  CastlePrivileges {
      whiteKingCastleAvailable = T.isInfixOf "K" castlePart
    , whiteQueenCastleAvailable = T.isInfixOf "Q" castlePart
    , blackKingCastleAvailable = T.isInfixOf "k" castlePart
    , blackQueenCastleAvailable = T.isInfixOf "q" castlePart
  } where castlePart = T.pack (fenPart fen 2)

getPosition :: String -> Position
getPosition fen = Position {
    whitePawnBitboard = pieceBitboard fenRanks 'P'
  , whiteKnightBitboard = pieceBitboard fenRanks 'N'
  , whiteBishopBitboard = pieceBitboard fenRanks 'B'
  , whiteQueenBitboard = pieceBitboard fenRanks 'Q'
  , whiteKingBitboard = pieceBitboard fenRanks 'K'
  , whiteRookBitboard = pieceBitboard fenRanks 'R'
  , blackPawnBitboard = pieceBitboard fenRanks 'p'
  , blackKnightBitboard = pieceBitboard fenRanks 'n'
  , blackBishopBitboard = pieceBitboard fenRanks 'b'
  , blackQueenBitboard = pieceBitboard fenRanks 'q'
  , blackKingBitboard = pieceBitboard fenRanks 'k'
  , blackRookBitboard = pieceBitboard fenRanks 'r'
  , mover = getMover fen
  , enPassantSquare = enPassantBitRef (enpassantFenPart fen)
  , positionCastlePrivs = getCastlePrivs fen
  , halfMoves = read (fenPart fen 4) :: Int
  , moveNumber = read (fenPart fen 5) :: Int
} where fenRanks = getFenRanks (fenBoardPart fen)
