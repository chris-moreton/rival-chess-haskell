{-# LANGUAGE BinaryLiterals,NegativeLiterals,StrictData,BangPatterns #-}

{-# OPTIONS_GHC -Wno-overflowed-literals #-}

module Search.MakeSimpleMove where

import Types
import Alias
import Util.Fen
import Util.Utils
import Data.Bits
import Util.Bitboards
import Search.MoveConstants
import Search.MoveUtils
import Search.MakeComplexMove

makeSimpleMove :: Position -> Move -> Square -> Position
makeSimpleMove !position !move !from
    | testBit (whitePiecesBitboard position) from = makeSimpleWhiteMove position move from
    | otherwise = makeSimpleBlackMove position move from

makeSimpleWhiteMove :: Position -> Move -> Square -> Position
makeSimpleWhiteMove !position !move !from
    | testBit (whiteKnightBitboard position) from = makeSimpleWhiteKnightMove position move
    | testBit (whiteBishopBitboard position) from = makeSimpleWhiteBishopMove position move
    | testBit (whiteRookBitboard position) from = makeSimpleWhiteRookMove position move
    | testBit (whiteQueenBitboard position) from = makeSimpleWhiteQueenMove position move
    | otherwise = makeMoveMain position move   

makeSimpleBlackMove :: Position -> Move -> Square -> Position
makeSimpleBlackMove !position !move !from
    | testBit (blackKnightBitboard position) from = makeSimpleBlackKnightMove position move
    | testBit (blackBishopBitboard position) from = makeSimpleBlackBishopMove position move
    | testBit (blackRookBitboard position) from = makeSimpleBlackRookMove position move
    | testBit (blackQueenBitboard position) from = makeSimpleBlackQueenMove position move
    | otherwise = makeMoveMain position move   

makeSimpleWhiteKnightMove :: Position -> Move -> Position
makeSimpleWhiteKnightMove !position !move =
    position {
          whiteKnightBitboard = movePieceWithinBitboard from to (whiteKnightBitboard position)
        , allPiecesBitboard = (allPiecesBitboard position) `xor`  switchBitboard
        , whitePiecesBitboard = (whitePiecesBitboard position) `xor`  switchBitboard
        , mover = Black
        , enPassantSquare = enPassantNotAvailable
        , halfMoves = halfMoves position + 1
    }
    where from = fromSquarePart move
          to = toSquarePart move
          switchBitboard = bit from .|. bit to

makeSimpleWhiteBishopMove :: Position -> Move -> Position
makeSimpleWhiteBishopMove !position !move =
    position {
          whiteBishopBitboard = movePieceWithinBitboard from to (whiteBishopBitboard position)
        , allPiecesBitboard = (allPiecesBitboard position) `xor`  switchBitboard
        , whitePiecesBitboard = (whitePiecesBitboard position) `xor`  switchBitboard
        , mover = Black
        , enPassantSquare = enPassantNotAvailable
        , halfMoves = halfMoves position + 1
    }
    where from = fromSquarePart move
          to = toSquarePart move
          switchBitboard = bit from .|. bit to

makeSimpleWhiteRookMove :: Position -> Move -> Position
makeSimpleWhiteRookMove !position !move =
    position {
          whiteRookBitboard = movePieceWithinBitboard from to (whiteRookBitboard position)    
        , allPiecesBitboard = (allPiecesBitboard position) `xor`  switchBitboard
        , whitePiecesBitboard = (whitePiecesBitboard position) `xor`  switchBitboard
        , mover = Black
        , enPassantSquare = enPassantNotAvailable
        , whiteKingCastleAvailable = whiteKingCastleAvailable position && from /= h1Bit
        , whiteQueenCastleAvailable = whiteQueenCastleAvailable position && from /= a1Bit        
        , halfMoves = halfMoves position + 1
    }
    where from = fromSquarePart move
          to = toSquarePart move
          switchBitboard = bit from .|. bit to

makeSimpleWhiteQueenMove :: Position -> Move -> Position
makeSimpleWhiteQueenMove !position !move =
    position {
          whiteQueenBitboard = movePieceWithinBitboard from to (whiteQueenBitboard position)  
        , allPiecesBitboard = (allPiecesBitboard position) `xor`  switchBitboard
        , whitePiecesBitboard = (whitePiecesBitboard position) `xor`  switchBitboard
        , mover = Black
        , enPassantSquare = enPassantNotAvailable
        , halfMoves = halfMoves position + 1
    }
    where from = fromSquarePart move
          to = toSquarePart move
          switchBitboard = bit from .|. bit to

makeSimpleBlackKnightMove :: Position -> Move -> Position
makeSimpleBlackKnightMove !position !move =
    position {
          blackKnightBitboard = movePieceWithinBitboard from to (blackKnightBitboard position)
        , allPiecesBitboard = (allPiecesBitboard position) `xor`  switchBitboard
        , blackPiecesBitboard = (blackPiecesBitboard position) `xor`  switchBitboard
        , mover = White
        , enPassantSquare = enPassantNotAvailable
        , halfMoves = halfMoves position + 1
        , moveNumber = moveNumber position + 1
    }
    where from = fromSquarePart move
          to = toSquarePart move
          switchBitboard = bit from .|. bit to

makeSimpleBlackBishopMove :: Position -> Move -> Position
makeSimpleBlackBishopMove !position !move =
    position {
          blackBishopBitboard = movePieceWithinBitboard from to (blackBishopBitboard position)
        , allPiecesBitboard = (allPiecesBitboard position) `xor` switchBitboard
        , blackPiecesBitboard = (blackPiecesBitboard position) `xor` switchBitboard
        , mover = White
        , enPassantSquare = enPassantNotAvailable
        , halfMoves = halfMoves position + 1
        , moveNumber = moveNumber position + 1
    }
    where from = fromSquarePart move
          to = toSquarePart move
          switchBitboard = bit from .|. bit to

makeSimpleBlackRookMove :: Position -> Move -> Position
makeSimpleBlackRookMove !position !move =
    position {
          blackRookBitboard = movePieceWithinBitboard from to (blackRookBitboard position)    
        , allPiecesBitboard = (allPiecesBitboard position) `xor` switchBitboard
        , blackPiecesBitboard = (blackPiecesBitboard position) `xor` switchBitboard
        , mover = White
        , enPassantSquare = enPassantNotAvailable
        , blackKingCastleAvailable = blackKingCastleAvailable position && from /= h8Bit
        , blackQueenCastleAvailable = blackQueenCastleAvailable position && from /= a8Bit        
        , halfMoves = halfMoves position + 1
        , moveNumber = moveNumber position + 1
    }
    where from = fromSquarePart move
          to = toSquarePart move
          switchBitboard = bit from .|. bit to

makeSimpleBlackQueenMove :: Position -> Move -> Position
makeSimpleBlackQueenMove !position !move =
    position {
          blackQueenBitboard = movePieceWithinBitboard from to (blackQueenBitboard position)  
        , allPiecesBitboard = (allPiecesBitboard position) `xor` switchBitboard
        , blackPiecesBitboard = (blackPiecesBitboard position) `xor` switchBitboard
        , mover = White
        , enPassantSquare = enPassantNotAvailable
        , halfMoves = halfMoves position + 1
        , moveNumber = moveNumber position + 1
    }
    where from = fromSquarePart move
          to = toSquarePart move
          switchBitboard = bit from .|. bit to
