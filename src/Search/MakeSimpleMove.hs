{-# LANGUAGE BinaryLiterals,NegativeLiterals,StrictData,BangPatterns #-}

{-# OPTIONS_GHC -Wno-overflowed-literals #-}

module Search.MakeSimpleMove where

import Types
    ( Mover(White, Black),
      Piece(Queen, Pawn, Knight, Bishop, Rook),
      Position(whitePawnBitboard, whiteKnightBitboard,
               whiteBishopBitboard, whiteRookBitboard, whiteKingCastleAvailable,
               whiteQueenCastleAvailable, whiteQueenBitboard, whiteKingBitboard,
               whitePiecesBitboard, blackPawnBitboard, blackKnightBitboard,
               blackBishopBitboard, blackRookBitboard, blackKingCastleAvailable,
               blackQueenCastleAvailable, blackQueenBitboard, blackKingBitboard,
               allPiecesBitboard, blackPiecesBitboard, mover, enPassantSquare,
               halfMoves, moveNumber) )
import Alias ( Move, Bitboard, Square )
import Util.Fen ()
import Util.Utils ( toSquarePart )
import Data.Bits ( Bits(xor, bit, (.|.)) )
import Util.Bitboards ( a1Bit, a8Bit, h1Bit, h8Bit )
import Search.MoveConstants ( enPassantNotAvailable )
import Search.MoveUtils ( movePieceWithinBitboard )
import Search.MakeComplexMove ()

makeSimpleMove :: Position -> Move -> Square -> Piece -> Position
makeSimpleMove !position !move !from !piece
    | mover position == White = makeSimpleWhiteMove position from to switchBitboard piece
    | otherwise = makeSimpleBlackMove position from to switchBitboard piece
    where !to = toSquarePart move
          !switchBitboard = bit from .|. bit to

makeSimpleWhiteMove :: Position -> Square -> Square -> Bitboard -> Piece -> Position
makeSimpleWhiteMove !position !from !to !switchBitboard !piece
    | piece == Pawn = makeSimpleWhitePawnMove position from to switchBitboard
    | piece == Knight = makeSimpleWhiteKnightMove position from to switchBitboard
    | piece == Bishop = makeSimpleWhiteBishopMove position from to switchBitboard
    | piece == Rook = makeSimpleWhiteRookMove position from to switchBitboard
    | piece == Queen = makeSimpleWhiteQueenMove position from to switchBitboard
    | otherwise = makeSimpleWhiteKingMove position from to switchBitboard

makeSimpleBlackMove :: Position -> Square -> Square -> Bitboard -> Piece -> Position
makeSimpleBlackMove !position !from !to !switchBitboard !piece
    | piece == Pawn = makeSimpleBlackPawnMove position from to switchBitboard
    | piece == Knight = makeSimpleBlackKnightMove position from to switchBitboard
    | piece == Bishop = makeSimpleBlackBishopMove position from to switchBitboard
    | piece == Rook = makeSimpleBlackRookMove position from to switchBitboard
    | piece == Queen = makeSimpleBlackQueenMove position from to switchBitboard
    | otherwise = makeSimpleBlackKingMove position from to switchBitboard

makeSimpleWhitePawnMove :: Position -> Square -> Square -> Bitboard -> Position
makeSimpleWhitePawnMove !position !from !to !switchBitboard =
    position {
          whitePawnBitboard = movePieceWithinBitboard from to (whitePawnBitboard position)
        , allPiecesBitboard = allPiecesBitboard position `xor` switchBitboard
        , whitePiecesBitboard = whitePiecesBitboard position `xor` switchBitboard
        , mover = Black
        , enPassantSquare = if to - from == 16 then from + 8 else enPassantNotAvailable
        , halfMoves = 0
    }

makeSimpleWhiteKnightMove :: Position -> Square -> Square -> Bitboard -> Position
makeSimpleWhiteKnightMove !position !from !to !switchBitboard =
    position {
          whiteKnightBitboard = movePieceWithinBitboard from to (whiteKnightBitboard position)
        , allPiecesBitboard = allPiecesBitboard position `xor`  switchBitboard
        , whitePiecesBitboard = whitePiecesBitboard position `xor`  switchBitboard
        , mover = Black
        , enPassantSquare = enPassantNotAvailable
        , halfMoves = halfMoves position + 1
    }

makeSimpleWhiteBishopMove :: Position -> Square -> Square -> Bitboard -> Position
makeSimpleWhiteBishopMove !position !from !to !switchBitboard =
    position {
          whiteBishopBitboard = movePieceWithinBitboard from to (whiteBishopBitboard position)
        , allPiecesBitboard = allPiecesBitboard position `xor`  switchBitboard
        , whitePiecesBitboard = whitePiecesBitboard position `xor`  switchBitboard
        , mover = Black
        , enPassantSquare = enPassantNotAvailable
        , halfMoves = halfMoves position + 1
    }

makeSimpleWhiteRookMove :: Position -> Square -> Square -> Bitboard -> Position
makeSimpleWhiteRookMove !position !from !to !switchBitboard =
    position {
          whiteRookBitboard = movePieceWithinBitboard from to (whiteRookBitboard position)    
        , allPiecesBitboard = allPiecesBitboard position `xor`  switchBitboard
        , whitePiecesBitboard = whitePiecesBitboard position `xor`  switchBitboard
        , mover = Black
        , enPassantSquare = enPassantNotAvailable
        , whiteKingCastleAvailable = whiteKingCastleAvailable position && from /= h1Bit
        , whiteQueenCastleAvailable = whiteQueenCastleAvailable position && from /= a1Bit        
        , halfMoves = halfMoves position + 1
    }

makeSimpleWhiteQueenMove :: Position -> Square -> Square -> Bitboard -> Position
makeSimpleWhiteQueenMove !position !from !to !switchBitboard =
    position {
          whiteQueenBitboard = movePieceWithinBitboard from to (whiteQueenBitboard position)  
        , allPiecesBitboard = allPiecesBitboard position `xor` switchBitboard
        , whitePiecesBitboard = whitePiecesBitboard position `xor` switchBitboard
        , mover = Black
        , enPassantSquare = enPassantNotAvailable
        , halfMoves = halfMoves position + 1
    }

makeSimpleWhiteKingMove :: Position -> Square -> Square -> Bitboard -> Position
makeSimpleWhiteKingMove !position !from !to !switchBitboard =
    position {
          whiteKingBitboard = movePieceWithinBitboard from to (whiteKingBitboard position)  
        , allPiecesBitboard = allPiecesBitboard position `xor` switchBitboard
        , whitePiecesBitboard = whitePiecesBitboard position `xor` switchBitboard
        , mover = Black
        , enPassantSquare = enPassantNotAvailable
        , halfMoves = halfMoves position + 1
    }

makeSimpleBlackPawnMove :: Position -> Square -> Square -> Bitboard -> Position
makeSimpleBlackPawnMove !position !from !to !switchBitboard =
    position {
          blackPawnBitboard = movePieceWithinBitboard from to (blackPawnBitboard position)
        , allPiecesBitboard = allPiecesBitboard position `xor` switchBitboard
        , blackPiecesBitboard = blackPiecesBitboard position `xor` switchBitboard
        , mover = White
        , enPassantSquare = if from - to == 16 then from - 8 else enPassantNotAvailable
        , halfMoves = 0
        , moveNumber = moveNumber position + 1
    }

makeSimpleBlackKnightMove :: Position -> Square -> Square -> Bitboard -> Position
makeSimpleBlackKnightMove !position !from !to !switchBitboard =
    position {
          blackKnightBitboard = movePieceWithinBitboard from to (blackKnightBitboard position)
        , allPiecesBitboard = allPiecesBitboard position `xor` switchBitboard
        , blackPiecesBitboard = blackPiecesBitboard position `xor` switchBitboard
        , mover = White
        , enPassantSquare = enPassantNotAvailable
        , halfMoves = halfMoves position + 1
        , moveNumber = moveNumber position + 1
    }

makeSimpleBlackBishopMove :: Position -> Square -> Square -> Bitboard -> Position
makeSimpleBlackBishopMove !position !from !to !switchBitboard =
    position {
          blackBishopBitboard = movePieceWithinBitboard from to (blackBishopBitboard position)
        , allPiecesBitboard = allPiecesBitboard position `xor` switchBitboard
        , blackPiecesBitboard = blackPiecesBitboard position `xor` switchBitboard
        , mover = White
        , enPassantSquare = enPassantNotAvailable
        , halfMoves = halfMoves position + 1
        , moveNumber = moveNumber position + 1
    }

makeSimpleBlackRookMove :: Position -> Square -> Square -> Bitboard -> Position
makeSimpleBlackRookMove !position !from !to !switchBitboard =
    position {
          blackRookBitboard = movePieceWithinBitboard from to (blackRookBitboard position)    
        , allPiecesBitboard = allPiecesBitboard position `xor` switchBitboard
        , blackPiecesBitboard = blackPiecesBitboard position `xor` switchBitboard
        , mover = White
        , enPassantSquare = enPassantNotAvailable
        , blackKingCastleAvailable = blackKingCastleAvailable position && from /= h8Bit
        , blackQueenCastleAvailable = blackQueenCastleAvailable position && from /= a8Bit        
        , halfMoves = halfMoves position + 1
        , moveNumber = moveNumber position + 1
    }

makeSimpleBlackQueenMove :: Position -> Square -> Square -> Bitboard -> Position
makeSimpleBlackQueenMove !position !from !to !switchBitboard =
    position {
          blackQueenBitboard = movePieceWithinBitboard from to (blackQueenBitboard position)  
        , allPiecesBitboard = allPiecesBitboard position `xor` switchBitboard
        , blackPiecesBitboard = blackPiecesBitboard position `xor` switchBitboard
        , mover = White
        , enPassantSquare = enPassantNotAvailable
        , halfMoves = halfMoves position + 1
        , moveNumber = moveNumber position + 1
    }

makeSimpleBlackKingMove :: Position -> Square -> Square -> Bitboard -> Position
makeSimpleBlackKingMove !position !from !to !switchBitboard =
    position {
          blackKingBitboard = movePieceWithinBitboard from to (blackKingBitboard position)  
        , allPiecesBitboard = allPiecesBitboard position `xor` switchBitboard
        , blackPiecesBitboard = blackPiecesBitboard position `xor` switchBitboard
        , mover = White
        , enPassantSquare = enPassantNotAvailable
        , halfMoves = halfMoves position + 1
        , moveNumber = moveNumber position + 1
    }
