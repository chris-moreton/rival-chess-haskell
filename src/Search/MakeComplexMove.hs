{-# LANGUAGE BinaryLiterals,NegativeLiterals,StrictData,BangPatterns #-}

{-# OPTIONS_GHC -Wno-overflowed-literals #-}

module Search.MakeComplexMove where

import Types
import Alias
import Util.Fen
import Util.Utils
import Data.Bits
import Util.Bitboards
import Search.MoveConstants
import Search.MoveUtils

makeMoveMain :: Position -> Move -> Position
{-# INLINE makeMoveMain #-}
makeMoveMain !position !move =
    Position {
          whitePawnBitboard = wp
        , blackPawnBitboard = bp
        , whiteKnightBitboard = wn
        , blackKnightBitboard = bn
        , whiteBishopBitboard = wb
        , blackBishopBitboard = bb
        , whiteRookBitboard = wr
        , blackRookBitboard = br
        , whiteQueenBitboard = wq
        , blackQueenBitboard = bq
        , whiteKingBitboard = wk
        , blackKingBitboard = bk
        , allPiecesBitboard = wpb .|. bpb
        , whitePiecesBitboard = wpb
        , blackPiecesBitboard = bpb
        , allBitsExceptFriendlyPieces = complement (if m == White then bpb else wpb)
        , mover = if m == White then Black else White
        , enPassantSquare = enPassantNotAvailable
        , whiteKingCastleAvailable = whiteKingCastleAvailable position && from /= e1Bit && from /= h1Bit && to /= h1Bit
        , whiteQueenCastleAvailable = whiteQueenCastleAvailable position && from /= e1Bit && from /= a1Bit && to /= a1Bit
        , blackKingCastleAvailable = blackKingCastleAvailable position && from /= e8Bit && from /= h8Bit && to /= h8Bit
        , blackQueenCastleAvailable = blackQueenCastleAvailable position && from /= e8Bit && from /= a8Bit && to /= a8Bit
        , halfMoves = if testBit (allPiecesBitboard position) to || isPawnMove then 0 else halfMoves position + 1
        , moveNumber = (+) (moveNumber position) (if m == Black then 1 else 0)
    }
    where !from = fromSquarePart move
          !to = toSquarePart move
          !promotionPiece = promotionPieceFromMove move
          !m = mover position
          !newWhitePawnBitboard = movePieceWithinBitboard from to (whitePawnBitboard position)
          !newBlackPawnBitboard = movePieceWithinBitboard from to (blackPawnBitboard position)
          !isPawnMove = newWhitePawnBitboard /= whitePawnBitboard position || newBlackPawnBitboard /= blackPawnBitboard position
          !wp = removePawnIfPromotion (removePawnWhenEnPassant newBlackPawnBitboard newWhitePawnBitboard to (enPassantSquare position))
          !bp = removePawnIfPromotion (removePawnWhenEnPassant newWhitePawnBitboard newBlackPawnBitboard to (enPassantSquare position))
          !wn = createIfPromotion (promotionPiece == Knight) (whitePawnBitboard position) (movePieceWithinBitboard from to (whiteKnightBitboard position)) from to
          !bn = createIfPromotion (promotionPiece == Knight) (blackPawnBitboard position) (movePieceWithinBitboard from to (blackKnightBitboard position)) from to
          !wb = createIfPromotion (promotionPiece == Bishop) (whitePawnBitboard position) (movePieceWithinBitboard from to (whiteBishopBitboard position)) from to
          !bb = createIfPromotion (promotionPiece == Bishop) (blackPawnBitboard position) (movePieceWithinBitboard from to (blackBishopBitboard position)) from to
          !wr = createIfPromotion (promotionPiece == Rook) (whitePawnBitboard position) (moveWhiteRookWhenCastling from to (whiteKingBitboard position) (movePieceWithinBitboard from to (whiteRookBitboard position))) from to
          !br = createIfPromotion (promotionPiece == Rook) (blackPawnBitboard position) (moveBlackRookWhenCastling from to (blackKingBitboard position) (movePieceWithinBitboard from to (blackRookBitboard position))) from to
          !wq = createIfPromotion (promotionPiece == Queen) (whitePawnBitboard position) (movePieceWithinBitboard from to (whiteQueenBitboard position)) from to
          !bq = createIfPromotion (promotionPiece == Queen) (blackPawnBitboard position) (movePieceWithinBitboard from to (blackQueenBitboard position)) from to
          !wk = movePieceWithinBitboard from to (whiteKingBitboard position)
          !bk = movePieceWithinBitboard from to (blackKingBitboard position)          
          !wpb = wp .|. wn .|. wr .|. wk .|. wq .|. wb
          !bpb = bp .|. bn .|. br .|. bk .|. bq .|. bb