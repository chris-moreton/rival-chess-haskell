{-# LANGUAGE BinaryLiterals #-}

module Search.Evaluate where

import Types
    ( bitboardForColour,
      Mover(..),
      Piece(..),
      Position(mover, blackPiecesBitboard, whitePiecesBitboard,
               enPassantSquare, whitePawnBitboard, blackPawnBitboard,
               whiteKnightBitboard, blackKnightBitboard, whiteBishopBitboard,
               blackBishopBitboard, whiteRookBitboard, blackRookBitboard,
               whiteQueenBitboard, blackQueenBitboard) )
import Alias ( Bitboard, Move )
import Util.Utils ( toSquarePart )
import Data.Bits ( Bits(popCount), Bits(testBit), Bits(bit), (.|.), (.&.), clearBit, shiftL )

captureScore :: Position -> Move -> Int
captureScore position move
    | isCapture position move = pieceValue (capturePiece position move)
    | otherwise = 0

centreScore :: Position -> Move -> Int
centreScore position move
    | 0b0000000000000000001111000011110000111100001111000000000000000000 .&. toSquareMask /= 0 = 25
    | 0b0000000001111110010000100100001001000010010000100111111000000000 .&. toSquareMask /= 0 = 10
    | otherwise = 0
    where toSquareMask = bit (toSquarePart move) :: Bitboard

scoreMove :: Position -> Move -> Int
scoreMove position move = captureScore position move + centreScore position move

pieceValue :: Piece -> Int
pieceValue Pawn = 100
pieceValue Knight = 350
pieceValue Bishop = 350
pieceValue Rook = 500
pieceValue Queen = 900
pieceValue King = 3000

{-# INLINE material #-}
material :: Position -> Mover -> Int
material position m
    | m == White = mw
    | otherwise = -mw
    where mw = materialWhite position

{-# INLINE materialWhite #-}
materialWhite :: Position -> Int 
materialWhite position =
    (popCount (whitePawnBitboard position) - popCount (blackPawnBitboard position)) * pieceValue Pawn +
    (popCount (whiteKnightBitboard position) - popCount (blackKnightBitboard position)) * pieceValue Knight +
    (popCount (whiteBishopBitboard position) - popCount (blackBishopBitboard position)) * pieceValue Bishop +
    (popCount (whiteRookBitboard position) - popCount (blackRookBitboard position)) * pieceValue Rook +
    (popCount (whiteQueenBitboard position) - popCount (blackQueenBitboard position)) * pieceValue Queen

{-# INLINE evaluate #-}
evaluate :: Position -> Int
evaluate position = material position (mover position)

{-# INLINE isCapture #-}
isCapture :: Position -> Move -> Bool
isCapture position move
    | m == White = testBit (blackPiecesBitboard position) t || e == t
    | otherwise = testBit (whitePiecesBitboard position) t || e == t
    where m = mover position
          t = toSquarePart move
          e = enPassantSquare position

{-# INLINE capturePiece #-}
capturePiece :: Position -> Move -> Piece
capturePiece position move
    | e == t = Pawn
    | testBit (whitePawnBitboard position) t = Pawn
    | testBit (blackPawnBitboard position) t = Pawn
    | testBit (whiteKnightBitboard position) t = Knight
    | testBit (blackKnightBitboard position) t = Knight
    | testBit (whiteBishopBitboard position) t = Bishop
    | testBit (blackBishopBitboard position) t = Bishop
    | testBit (whiteRookBitboard position) t = Rook
    | testBit (blackRookBitboard position) t = Rook
    | testBit (whiteQueenBitboard position) t = Queen
    | testBit (blackQueenBitboard position) t = Queen
    where t = toSquarePart move
          e = enPassantSquare position          