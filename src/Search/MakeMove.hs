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

removePieceFromBitboard :: Square -> Bitboard -> Bitboard
{-# INLINE removePieceFromBitboard #-}
removePieceFromBitboard !square = (.&.) (complement (bit square))

moveWhiteRookWhenCastling :: Square -> Square -> Bitboard -> Bitboard -> Bitboard
{-# INLINE moveWhiteRookWhenCastling #-}
moveWhiteRookWhenCastling !from !to !kingBoard !rookBoard
  | not (testBit kingBoard e1Bit) = rookBoard
  | from == e1Bit && to == g1Bit = movePieceWithinBitboard h1Bit f1Bit rookBoard
  | from == e1Bit && to == c1Bit = movePieceWithinBitboard a1Bit d1Bit rookBoard
  | otherwise = rookBoard

moveBlackRookWhenCastling :: Square -> Square -> Bitboard -> Bitboard -> Bitboard
{-# INLINE moveBlackRookWhenCastling #-}
moveBlackRookWhenCastling !from !to !kingBoard !rookBoard
  | not (testBit kingBoard e8Bit) = rookBoard
  | from == e8Bit && to == g8Bit = movePieceWithinBitboard h8Bit f8Bit rookBoard
  | from == e8Bit && to == c8Bit = movePieceWithinBitboard a8Bit d8Bit rookBoard
  | otherwise = rookBoard

enPassantCapturedPieceSquare :: Square -> Square
{-# INLINE enPassantCapturedPieceSquare #-}
enPassantCapturedPieceSquare !enPassantSquare
  | enPassantSquare < 24 = enPassantSquare + 8
  | otherwise = enPassantSquare - 8

removePawnWhenEnPassant :: Bitboard -> Bitboard -> Square -> Square -> Bitboard
{-# INLINE removePawnWhenEnPassant #-}
removePawnWhenEnPassant !attackerBb !defenderBb !to !enPassantSquare
  | enPassantSquare == to && testBit attackerBb to = removePieceFromBitboard (enPassantCapturedPieceSquare to) defenderBb
  | otherwise = defenderBb

removePawnIfPromotion :: Bitboard -> Bitboard
{-# INLINE removePawnIfPromotion #-}
removePawnIfPromotion !bb = bb .&. 0b0000000011111111111111111111111111111111111111111111111100000000

isPromotionSquare :: Square -> Bool
{-# INLINE isPromotionSquare #-}
isPromotionSquare !sq = (bit sq .&. promotionSquares) /= 0

createIfPromotion :: Bool -> Bitboard -> Bitboard -> Square -> Square -> Bitboard
{-# INLINE createIfPromotion #-}
createIfPromotion !isPromotionPiece !pawnBitboard !pieceBitboard !fromSquare !toSquare
  | isPromotionPiece && isPromotionSquare toSquare && testBit pawnBitboard fromSquare = pieceBitboard .|. bit toSquare
  | otherwise = pieceBitboard

movePieceWithinBitboard :: Square -> Square -> Bitboard -> Bitboard
{-# INLINE movePieceWithinBitboard #-}
movePieceWithinBitboard !from !to !bb
  | testBit bb from = (.|.) (clearBit bb from) (bit to)
  | otherwise = clearBit bb to

makeMove :: Position -> Move -> Position
makeMove !position !move = 
    if not (testBit (allPiecesBitboard position) to) 
        then if testBit ((whiteBishopBitboard position) .|. (whiteKnightBitboard position) .|. (whiteQueenBitboard position) .|. (whiteRookBitboard position)) from
               then makeSimpleWhiteMove position move
               else if testBit ((blackBishopBitboard position) .|. (blackKnightBitboard position) .|. (blackQueenBitboard position) .|. (blackRookBitboard position)) from
                      then makeSimpleBlackMove position move
                      else makeMoveMain position move             
        else makeMoveMain position move
    where from = fromSquarePart move
          to = toSquarePart move
          promotionPiece = promotionPieceFromMove move
          m = mover position

makeSimpleWhiteMove :: Position -> Move -> Position
makeSimpleWhiteMove !position !move =
    position {
          whiteKnightBitboard = wn
        , whiteBishopBitboard = wb
        , whiteRookBitboard = wr
        , whiteQueenBitboard = wq
        , allPiecesBitboard = wpb .|. (blackPiecesBitboard position)
        , whitePiecesBitboard = wpb
        , mover = Black
        , enPassantSquare = enPassantNotAvailable
        , whiteKingCastleAvailable = whiteKingCastleAvailable position && from /= h1Bit
        , whiteQueenCastleAvailable = whiteQueenCastleAvailable position && from /= a1Bit
        , halfMoves = halfMoves position + 1
    }
    where from = fromSquarePart move
          to = toSquarePart move
          wp = whitePawnBitboard position
          wn = movePieceWithinBitboard from to (whiteKnightBitboard position)
          wb = movePieceWithinBitboard from to (whiteBishopBitboard position)
          wr = movePieceWithinBitboard from to (whiteRookBitboard position)
          wq = movePieceWithinBitboard from to (whiteQueenBitboard position)
          wk = whiteKingBitboard position
          wpb = wp .|. wn .|. wr .|. wk .|. wq .|. wb

makeSimpleBlackMove :: Position -> Move -> Position
makeSimpleBlackMove !position !move =
    position {
          blackKnightBitboard = bn
        , blackBishopBitboard = bb
        , blackRookBitboard = br
        , blackQueenBitboard = bq
        , allPiecesBitboard = bpb .|. (whitePiecesBitboard position)
        , blackPiecesBitboard = bpb
        , mover = White
        , enPassantSquare = enPassantNotAvailable
        , blackKingCastleAvailable = blackKingCastleAvailable position && from /= h8Bit
        , blackQueenCastleAvailable = blackQueenCastleAvailable position && from /= a8Bit
        , halfMoves = halfMoves position + 1
        , moveNumber = moveNumber position + 1
    }
    where from = fromSquarePart move
          to = toSquarePart move
          bp = blackPawnBitboard position
          bn = movePieceWithinBitboard from to (blackKnightBitboard position)
          bb = movePieceWithinBitboard from to (blackBishopBitboard position)
          br = movePieceWithinBitboard from to (blackRookBitboard position)
          bq = movePieceWithinBitboard from to (blackQueenBitboard position)
          bk = blackKingBitboard position
          bpb = bp .|. bn .|. br .|. bk .|. bq .|. bb          

makeMoveMain :: Position -> Move -> Position
makeMoveMain !position !move =
    position {
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
        , mover = if m == White then Black else White
        , enPassantSquare = if (abs (to - from)) == 16 
                              then if (testBit (whitePawnBitboard position) from) 
                                     then from + 8 
                                     else if (testBit (blackPawnBitboard position) from) 
                                            then from - 8 
                                            else enPassantNotAvailable
                              else enPassantNotAvailable
        , whiteKingCastleAvailable = whiteKingCastleAvailable position && from /= e1Bit && from /= h1Bit && to /= h1Bit
        , whiteQueenCastleAvailable = whiteQueenCastleAvailable position && from /= e1Bit && from /= a1Bit && to /= a1Bit
        , blackKingCastleAvailable = blackKingCastleAvailable position && from /= e8Bit && from /= h8Bit && to /= h8Bit
        , blackQueenCastleAvailable = blackQueenCastleAvailable position && from /= e8Bit && from /= a8Bit && to /= a8Bit
        , halfMoves = if testBit (allPiecesBitboard position) to || isPawnMove then 0 else halfMoves position + 1
        , moveNumber = (+) (moveNumber position) (if m == Black then 1 else 0)
    }
    where from = fromSquarePart move
          to = toSquarePart move
          promotionPiece = promotionPieceFromMove move
          m = mover position
          newWhitePawnBitboard = movePieceWithinBitboard from to (whitePawnBitboard position)
          newBlackPawnBitboard = movePieceWithinBitboard from to (blackPawnBitboard position)
          isPawnMove = newWhitePawnBitboard /= whitePawnBitboard position || newBlackPawnBitboard /= blackPawnBitboard position
          wp = removePawnIfPromotion (removePawnWhenEnPassant newBlackPawnBitboard newWhitePawnBitboard to (enPassantSquare position))
          bp = removePawnIfPromotion (removePawnWhenEnPassant newWhitePawnBitboard newBlackPawnBitboard to (enPassantSquare position))
          wn = createIfPromotion (promotionPiece == Knight) (whitePawnBitboard position) (movePieceWithinBitboard from to (whiteKnightBitboard position)) from to
          bn = createIfPromotion (promotionPiece == Knight) (blackPawnBitboard position) (movePieceWithinBitboard from to (blackKnightBitboard position)) from to
          wb = createIfPromotion (promotionPiece == Bishop) (whitePawnBitboard position) (movePieceWithinBitboard from to (whiteBishopBitboard position)) from to
          bb = createIfPromotion (promotionPiece == Bishop) (blackPawnBitboard position) (movePieceWithinBitboard from to (blackBishopBitboard position)) from to
          wr = createIfPromotion (promotionPiece == Rook) (whitePawnBitboard position) (moveWhiteRookWhenCastling from to (whiteKingBitboard position) (movePieceWithinBitboard from to (whiteRookBitboard position))) from to
          br = createIfPromotion (promotionPiece == Rook) (blackPawnBitboard position) (moveBlackRookWhenCastling from to (blackKingBitboard position) (movePieceWithinBitboard from to (blackRookBitboard position))) from to
          wq = createIfPromotion (promotionPiece == Queen) (whitePawnBitboard position) (movePieceWithinBitboard from to (whiteQueenBitboard position)) from to
          bq = createIfPromotion (promotionPiece == Queen) (blackPawnBitboard position) (movePieceWithinBitboard from to (blackQueenBitboard position)) from to
          wk = movePieceWithinBitboard from to (whiteKingBitboard position)
          bk = movePieceWithinBitboard from to (blackKingBitboard position)          
          wpb = wp .|. wn .|. wr .|. wk .|. wq .|. wb
          bpb = bp .|. bn .|. br .|. bk .|. bq .|. bb



