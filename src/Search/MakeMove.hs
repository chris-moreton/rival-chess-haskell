{-# LANGUAGE BinaryLiterals,NegativeLiterals,StrictData,BangPatterns #-}

{-# OPTIONS_GHC -Wno-overflowed-literals #-}

module Search.MakeMove where

import Types
import Alias
import Util.Fen
import Util.Utils
import Data.Bits
import Util.Bitboards
import Search.MoveConstants
import Search.MakeSimpleMove
import Search.MoveUtils
import Search.MakeComplexMove

isPotentialFirstKingMove :: Position -> Square -> Bool
isPotentialFirstKingMove !position !from = from == e1Bit || from == e8Bit

isComplexPawnMove :: Position -> Square -> Square -> Bool
isComplexPawnMove !position !from !to = not (abs (from - to) `mod` 8 == 0) || testBit promotionSquares to

isSimpleCapture :: Position -> Square -> Bool
isSimpleCapture !position !to = testBit (allPiecesBitboard position) to

isSimpleMove :: Position -> Move -> Square -> Square -> Piece -> Bool
isSimpleMove !position !move !from !to !piece = not (isSimpleCapture position to) && not (piece == Pawn && isComplexPawnMove position from to) && not (piece == King && isPotentialFirstKingMove position from)

movingWhitePiece :: Position -> Square -> Piece
movingWhitePiece position from
    | testBit (whitePawnBitboard position) from = Pawn
    | testBit (whiteKnightBitboard position) from = Knight
    | testBit (whiteBishopBitboard position) from = Bishop
    | testBit (whiteRookBitboard position) from = Rook
    | testBit (whiteQueenBitboard position) from = Queen
    | otherwise = King

movingBlackPiece :: Position -> Square -> Piece
movingBlackPiece position from
    | testBit (blackPawnBitboard position) from = Pawn
    | testBit (blackKnightBitboard position) from = Knight
    | testBit (blackBishopBitboard position) from = Bishop
    | testBit (blackRookBitboard position) from = Rook
    | testBit (blackQueenBitboard position) from = Queen
    | otherwise = King

makeMove :: Position -> Move -> Position
makeMove !position !move = 
    if isSimpleMove position move from to piece
        then makeSimpleMove position move from piece
        else makeMoveMain position move
    where !from = fromSquarePart move
          !to = toSquarePart move
          !piece = if (mover position == White) then movingWhitePiece position from else movingBlackPiece position from





