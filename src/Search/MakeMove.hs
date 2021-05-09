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
isPotentialFirstKingMove !position from = from == e1Bit || from == e8Bit

isComplexPawnMove :: Position -> Square -> Square -> Bool
isComplexPawnMove !position from to = not (abs (from - to) `mod` 8 == 0) || testBit promotionSquares to

isSimpleCapture :: Position -> Square -> Bool
isSimpleCapture !position to = testBit (allPiecesBitboard position) to

isSimpleMove :: Position -> Move -> Square -> Square -> Piece -> Bool
isSimpleMove !position move from to piece = not (isSimpleCapture position to) && not (piece == Pawn && isComplexPawnMove position from to) && not (piece == King && isPotentialFirstKingMove position from)

movingPiece :: Position -> Square -> Piece
movingPiece position from
    | testBit (whitePawnBitboard position .|. blackPawnBitboard position) from = Pawn
    | testBit (whiteKnightBitboard position .|. blackKnightBitboard position) from = Knight
    | testBit (whiteBishopBitboard position .|. blackBishopBitboard position) from = Bishop
    | testBit (whiteRookBitboard position .|. blackRookBitboard position) from = Rook
    | testBit (whiteQueenBitboard position .|. blackQueenBitboard position) from = Queen
    | otherwise = King

makeMove :: Position -> Move -> Position
makeMove !position !move = 
    if isSimpleMove position move from to piece
        then makeSimpleMove position move from piece
        else makeMoveMain position move
    where !from = fromSquarePart move
          !to = toSquarePart move
          !piece = movingPiece position from





