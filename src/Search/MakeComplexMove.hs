{-# LANGUAGE BinaryLiterals,NegativeLiterals,StrictData,BangPatterns #-}

{-# OPTIONS_GHC -Wno-overflowed-literals #-}

module Search.MakeComplexMove where

import Types
    ( Position(..),
      Piece(Queen, Pawn, Knight, Bishop, Rook),
      Mover(Black, White) )
import Alias ( Move, Square )
import Util.Fen ()
import Util.Utils
    ( fromSquarePart,
      promotionPieceFromMove,
      switchSide,
      toSquarePart )
import Data.Bits ( Bits(testBit, (.|.)) )
import Util.Bitboards
    ( a1Bit,
      a8Bit,
      c1Bit,
      c8Bit,
      d1Bit,
      d8Bit,
      e1Bit,
      e8Bit,
      f1Bit,
      f8Bit,
      g1Bit,
      g8Bit,
      h1Bit,
      h8Bit )
import Search.MoveConstants ( enPassantNotAvailable )
import Search.MoveUtils
    ( createIfPromotion,
      moveBlackRookWhenCastling,
      movePieceWithinBitboard,
      moveWhiteRookWhenCastling,
      removePawnIfPromotion,
      removePawnWhenEnPassant )

makeComplexMove :: Position -> Move -> Position
{-# INLINE makeComplexMove #-}
makeComplexMove !position !move
    | pp /= Pawn = makeMoveWithPromotion position move pp
    | from == e1Bit && (to == g1Bit || to == c1Bit) && (whiteKingCastleAvailable position || whiteQueenCastleAvailable position) = makeWhiteCastleMove position to
    | from == e8Bit && (to == g8Bit || to == c8Bit) && (blackKingCastleAvailable position || blackQueenCastleAvailable position) = makeBlackCastleMove position to
    | otherwise = makeSimpleComplexMove position from to
    where !pp = promotionPieceFromMove move
          !from = fromSquarePart move
          !to = toSquarePart move

makeWhiteCastleMove :: Position -> Square -> Position
{-# INLINE makeWhiteCastleMove #-}
makeWhiteCastleMove !position !to =
    position {
          whiteRookBitboard = wr
        , whiteKingBitboard = wk
        , allPiecesBitboard = wpb .|. blackPiecesBitboard position
        , whitePiecesBitboard = wpb
        , mover = Black
        , enPassantSquare = enPassantNotAvailable
        , whiteKingCastleAvailable = False
        , whiteQueenCastleAvailable = False
        , halfMoves = halfMoves position + 1
    }
    where !wr = if to == c1Bit then movePieceWithinBitboard a1Bit d1Bit (whiteRookBitboard position) else movePieceWithinBitboard h1Bit f1Bit (whiteRookBitboard position)
          !wk = movePieceWithinBitboard e1Bit to (whiteKingBitboard position)
          !wpb = wr .|. wk .|. whiteQueenBitboard position .|. whiteKnightBitboard position .|. whiteBishopBitboard position .|. whitePawnBitboard position

makeBlackCastleMove :: Position -> Square -> Position
{-# INLINE makeBlackCastleMove #-}
makeBlackCastleMove !position !to =
    position {
          blackRookBitboard = br
        , blackKingBitboard = bk
        , allPiecesBitboard = bpb .|. whitePiecesBitboard position
        , blackPiecesBitboard = bpb
        , mover = White
        , enPassantSquare = enPassantNotAvailable
        , blackKingCastleAvailable = False
        , blackQueenCastleAvailable = False
        , halfMoves = halfMoves position + 1
        , moveNumber = moveNumber position + 1
    }
    where !br = if to == c8Bit then movePieceWithinBitboard a8Bit d8Bit (blackRookBitboard position) else movePieceWithinBitboard h8Bit f8Bit (blackRookBitboard position)
          !bk = movePieceWithinBitboard e8Bit to (blackKingBitboard position)
          !bpb = br .|. bk .|. blackQueenBitboard position .|. blackKnightBitboard position .|. blackBishopBitboard position .|. blackPawnBitboard position

makeMoveWithPromotion :: Position -> Move -> Piece -> Position
{-# INLINE makeMoveWithPromotion #-}
makeMoveWithPromotion !position !move !promotionPiece =
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
        , mover = switchSide m
        , enPassantSquare = enPassantNotAvailable
        , whiteKingCastleAvailable = whiteKingCastleAvailable position && to /= h1Bit
        , whiteQueenCastleAvailable = whiteQueenCastleAvailable position && to /= a1Bit
        , blackKingCastleAvailable = blackKingCastleAvailable position && to /= h8Bit
        , blackQueenCastleAvailable = blackQueenCastleAvailable position && to /= a8Bit
        , halfMoves = 0
        , moveNumber = if m == Black then currentMoveNumber + 1 else currentMoveNumber
    }
    where !from = fromSquarePart move
          !to = toSquarePart move
          !m = mover position
          currentMoveNumber = moveNumber position
          !wp = removePawnIfPromotion $ movePieceWithinBitboard from to (whitePawnBitboard position)
          !bp = removePawnIfPromotion $ movePieceWithinBitboard from to (blackPawnBitboard position)
          !wn = createIfPromotion (promotionPiece == Knight) (whitePawnBitboard position) (movePieceWithinBitboard from to (whiteKnightBitboard position)) from to
          !bn = createIfPromotion (promotionPiece == Knight) (blackPawnBitboard position) (movePieceWithinBitboard from to (blackKnightBitboard position)) from to
          !wb = createIfPromotion (promotionPiece == Bishop) (whitePawnBitboard position) (movePieceWithinBitboard from to (whiteBishopBitboard position)) from to
          !bb = createIfPromotion (promotionPiece == Bishop) (blackPawnBitboard position) (movePieceWithinBitboard from to (blackBishopBitboard position)) from to
          !wr = createIfPromotion (promotionPiece == Rook) (whitePawnBitboard position) (movePieceWithinBitboard from to (whiteRookBitboard position)) from to
          !br = createIfPromotion (promotionPiece == Rook) (blackPawnBitboard position) (movePieceWithinBitboard from to (blackRookBitboard position)) from to
          !wq = createIfPromotion (promotionPiece == Queen) (whitePawnBitboard position) (movePieceWithinBitboard from to (whiteQueenBitboard position)) from to
          !bq = createIfPromotion (promotionPiece == Queen) (blackPawnBitboard position) (movePieceWithinBitboard from to (blackQueenBitboard position)) from to
          !wk = movePieceWithinBitboard from to (whiteKingBitboard position)
          !bk = movePieceWithinBitboard from to (blackKingBitboard position)
          !wpb = wp .|. wn .|. wr .|. wk .|. wq .|. wb
          !bpb = bp .|. bn .|. br .|. bk .|. bq .|. bb

makeSimpleComplexMove :: Position -> Square -> Square -> Position
{-# INLINE makeSimpleComplexMove #-}
makeSimpleComplexMove !position !from !to =
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
        , mover = switchSide m
        , enPassantSquare = enPassantNotAvailable
        , whiteKingCastleAvailable = whiteKingCastleAvailable position && from /= e1Bit && from /= h1Bit && to /= h1Bit
        , whiteQueenCastleAvailable = whiteQueenCastleAvailable position && from /= e1Bit && from /= a1Bit && to /= a1Bit
        , blackKingCastleAvailable = blackKingCastleAvailable position && from /= e8Bit && from /= h8Bit && to /= h8Bit
        , blackQueenCastleAvailable = blackQueenCastleAvailable position && from /= e8Bit && from /= a8Bit && to /= a8Bit
        , halfMoves = if testBit (allPiecesBitboard position) to || isPawnMove then 0 else halfMoves position + 1
        , moveNumber = if m == Black then currentMoveNumber + 1 else currentMoveNumber
    }
    where currentMoveNumber = moveNumber position
          !m = mover position
          !newWhitePawnBitboard = movePieceWithinBitboard from to (whitePawnBitboard position)
          !newBlackPawnBitboard = movePieceWithinBitboard from to (blackPawnBitboard position)
          !isPawnMove = newWhitePawnBitboard /= whitePawnBitboard position || newBlackPawnBitboard /= blackPawnBitboard position
          !wp = removePawnWhenEnPassant newBlackPawnBitboard newWhitePawnBitboard to (enPassantSquare position)
          !bp = removePawnWhenEnPassant newWhitePawnBitboard newBlackPawnBitboard to (enPassantSquare position)
          !wn = movePieceWithinBitboard from to (whiteKnightBitboard position)
          !bn = movePieceWithinBitboard from to (blackKnightBitboard position)
          !wb = movePieceWithinBitboard from to (whiteBishopBitboard position)
          !bb = movePieceWithinBitboard from to (blackBishopBitboard position)
          !wr = moveWhiteRookWhenCastling from to (whiteKingBitboard position) (movePieceWithinBitboard from to (whiteRookBitboard position))
          !br = moveBlackRookWhenCastling from to (blackKingBitboard position) (movePieceWithinBitboard from to (blackRookBitboard position))
          !wq = movePieceWithinBitboard from to (whiteQueenBitboard position)
          !bq = movePieceWithinBitboard from to (blackQueenBitboard position)
          !wk = movePieceWithinBitboard from to (whiteKingBitboard position)
          !bk = movePieceWithinBitboard from to (blackKingBitboard position)
          !wpb = wp .|. wn .|. wr .|. wk .|. wq .|. wb
          !bpb = bp .|. bn .|. br .|. bk .|. bq .|. bb