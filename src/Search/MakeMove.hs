{-# LANGUAGE BinaryLiterals,NegativeLiterals,StrictData,BangPatterns #-}

{-# OPTIONS_GHC -Wno-overflowed-literals #-}

module Search.MakeMove (makeMove,makeAlgebraicMoves) where

import Types
    ( Position(allPiecesBitboard, whitePawnBitboard, blackPawnBitboard,
               whiteKnightBitboard, blackKnightBitboard, whiteBishopBitboard,
               blackBishopBitboard, whiteRookBitboard, blackRookBitboard,
               whiteQueenBitboard, blackQueenBitboard),
      Piece(..) )
import Alias ( Move, Square )
import Util.Fen ( moveFromAlgebraicMove )
import Util.Utils ( fromSquarePart, toSquarePart )
import Data.Bits ( Bits((.|.), testBit) )
import Util.Bitboards ( e1Bit, e8Bit, promotionSquares )
import Search.MoveConstants ()
import Search.MakeSimpleMove ( makeSimpleMove )
import Search.MoveUtils ()
import Search.MakeComplexMove ( makeComplexMove )

{-# INLINE isPotentialFirstKingMove #-}
isPotentialFirstKingMove :: Position -> Square -> Bool
isPotentialFirstKingMove !position from = from == e1Bit || from == e8Bit

{-# INLINE isComplexPawnMove #-}
isComplexPawnMove :: Position -> Square -> Square -> Bool
isComplexPawnMove !position from to = (abs (from - to) `mod` 8) /= 0 || testBit promotionSquares to

{-# INLINE isSimpleCapture #-}
isSimpleCapture :: Position -> Square -> Bool
isSimpleCapture !position = testBit (allPiecesBitboard position)

{-# INLINE isSimpleMove #-}
isSimpleMove :: Position -> Move -> Square -> Square -> Piece -> Bool
isSimpleMove !position move from to piece = 
    not (isSimpleCapture position to) && 
    not (piece == Pawn && isComplexPawnMove position from to) && 
    not (piece == King && isPotentialFirstKingMove position from)

{-# INLINE movingPiece #-}
movingPiece :: Position -> Square -> Piece
movingPiece position from
    | testBit (whitePawnBitboard position .|. blackPawnBitboard position) from = Pawn
    | testBit (whiteKnightBitboard position .|. blackKnightBitboard position) from = Knight
    | testBit (whiteBishopBitboard position .|. blackBishopBitboard position) from = Bishop
    | testBit (whiteRookBitboard position .|. blackRookBitboard position) from = Rook
    | testBit (whiteQueenBitboard position .|. blackQueenBitboard position) from = Queen
    | otherwise = King

makeAlgebraicMoves :: Position -> [String] -> Position
makeAlgebraicMoves = foldl (\ position move -> makeMove position $ moveFromAlgebraicMove move)

makeMove :: Position -> Move -> Position
makeMove !position !move =
    if isSimpleMove position move from to piece
        then makeSimpleMove position move from piece
        else makeComplexMove position move
    where !from  = fromSquarePart move
          !to    = toSquarePart move
          !piece = movingPiece position from





