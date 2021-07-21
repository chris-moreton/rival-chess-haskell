{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BinaryLiterals #-}

module Evaluate.Evaluate where

import Types
    ( bitboardForColour,
      Mover(..),
      Piece(..),
      Position(mover, blackPiecesBitboard, whitePiecesBitboard,
               enPassantSquare, whitePawnBitboard, blackPawnBitboard,
               whiteKnightBitboard, blackKnightBitboard, whiteBishopBitboard,
               blackBishopBitboard, whiteRookBitboard, blackRookBitboard,
               whiteQueenBitboard, blackQueenBitboard, whiteKingBitboard, blackKingBitboard) )
import Alias ( Bitboard, Move )
import Util.Utils ( toSquarePart )
import Util.Bitboards ( exactlyOneBitSet )
import Data.Bits ( Bits(popCount), Bits(testBit), Bits(bit), (.|.), (.&.), clearBit, shiftL )
import Evaluate.Attacks ( allAttacks )

{-# INLINE evaluate #-}
evaluate :: Position -> Int
evaluate !position 
    | onlyKingsRemain position = 0
    | otherwise = do
        let attacks = allAttacks position
        material position (mover position)


onlyKingsRemain :: Position -> Bool
onlyKingsRemain position = exactlyOneBitSet (whitePiecesBitboard position) && exactlyOneBitSet (blackPiecesBitboard position)

{-# INLINE captureScore #-}
captureScore :: Position -> Move -> Int
captureScore !position !move
    | isCapture position move = pieceValue (capturePiece position move)
    | otherwise = 0

{-# INLINE centreScore #-}
centreScore :: Position -> Move -> Int
centreScore !position !move
    | 0b0000000000000000001111000011110000111100001111000000000000000000 .&. toSquareMask /= 0 = 25
    | 0b0000000001111110010000100100001001000010010000100111111000000000 .&. toSquareMask /= 0 = 10
    | otherwise = 0
    where toSquareMask = bit (toSquarePart move) :: Bitboard

{-# INLINE scoreMove #-}
scoreMove :: Position -> Move -> Move -> Int
scoreMove !position !hashMove !move = captureScore position move + centreScore position move + (if hashMove == move then 100000 else 0)

{-# INLINE pieceValue #-}
pieceValue :: Piece -> Int
pieceValue Pawn = 100
pieceValue Knight = 350
pieceValue Bishop = 350
pieceValue Rook = 500
pieceValue Queen = 900
pieceValue King = 3000

{-# INLINE material #-}
material :: Position -> Mover -> Int
material !position !m
    | m == White = mw
    | otherwise = -mw
    where mw = materialWhite position

{-# INLINE materialWhite #-}
materialWhite :: Position -> Int
materialWhite !position =
    (popCount (whitePawnBitboard position) - popCount (blackPawnBitboard position)) * pieceValue Pawn +
    (popCount (whiteKnightBitboard position) - popCount (blackKnightBitboard position)) * pieceValue Knight +
    (popCount (whiteBishopBitboard position) - popCount (blackBishopBitboard position)) * pieceValue Bishop +
    (popCount (whiteRookBitboard position) - popCount (blackRookBitboard position)) * pieceValue Rook +
    (popCount (whiteQueenBitboard position) - popCount (blackQueenBitboard position)) * pieceValue Queen

{-# INLINE whitePieceValues #-}
whitePieceValues :: Position -> Int
whitePieceValues !position =
    popCount (whiteKnightBitboard position) * pieceValue Knight +
    popCount (whiteBishopBitboard position) * pieceValue Bishop +
    popCount (whiteRookBitboard position) * pieceValue Rook +
    popCount (whiteQueenBitboard position) * pieceValue Queen

{-# INLINE blackPieceValues #-}
blackPieceValues :: Position -> Int
blackPieceValues !position =
    popCount (blackKnightBitboard position) * pieceValue Knight +
    popCount (blackBishopBitboard position) * pieceValue Bishop +
    popCount (blackRookBitboard position) * pieceValue Rook +
    popCount (blackQueenBitboard position) * pieceValue Queen

{-# INLINE friendlyPieceValues #-}
friendlyPieceValues :: Position -> Int 
friendlyPieceValues !position
    | m == White = whitePieceValues position
    | m == Black = blackPieceValues position
    where m = mover position

{-# INLINE isCapture #-}
isCapture :: Position -> Move -> Bool
isCapture !position !move
    | m == White = testBit (blackPiecesBitboard position) t || ep == t
    | otherwise  = testBit (whitePiecesBitboard position) t || ep == t
    where m  = mover position
          t  = toSquarePart move
          ep = enPassantSquare position

{-# INLINE capturePiece #-}
capturePiece :: Position -> Move -> Piece
capturePiece !position !move
    | ep == t   = Pawn
    | otherwise = pieceOnSquareFast position t
    where t  = toSquarePart move
          ep = enPassantSquare position

{-# INLINE pieceOnSquareFast #-}
pieceOnSquareFast :: Position -> Int -> Piece
pieceOnSquareFast !position !square
    | testBit (whitePawnBitboard position) square = Pawn
    | testBit (blackPawnBitboard position) square = Pawn
    | testBit (whiteKnightBitboard position) square = Knight
    | testBit (blackKnightBitboard position) square = Knight
    | testBit (whiteBishopBitboard position) square = Bishop
    | testBit (blackBishopBitboard position) square = Bishop
    | testBit (whiteRookBitboard position) square = Rook
    | testBit (blackRookBitboard position) square = Rook
    | testBit (whiteQueenBitboard position) square = Queen
    | testBit (blackQueenBitboard position) square = Queen
    | testBit (whiteKingBitboard position) square = King
    | testBit (blackKingBitboard position) square = King

pieceOnSquare :: Position -> Int -> Maybe (Mover,Piece)
pieceOnSquare !position !square
    | testBit (whitePawnBitboard position) square = Just (White,Pawn)
    | testBit (blackPawnBitboard position) square = Just (Black,Pawn)
    | testBit (whiteKnightBitboard position) square = Just (White,Knight)
    | testBit (blackKnightBitboard position) square = Just (Black,Knight)
    | testBit (whiteBishopBitboard position) square = Just (White,Bishop)
    | testBit (blackBishopBitboard position) square = Just (Black,Bishop)
    | testBit (whiteRookBitboard position) square = Just (White,Rook)
    | testBit (blackRookBitboard position) square = Just (Black,Rook)
    | testBit (whiteQueenBitboard position) square = Just (White,Queen)
    | testBit (blackQueenBitboard position) square = Just (Black,Queen)
    | testBit (whiteKingBitboard position) square = Just (White,King)
    | testBit (blackKingBitboard position) square = Just (Black,King)
    | otherwise = Nothing      