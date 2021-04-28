{-# LANGUAGE BinaryLiterals,NegativeLiterals,StrictData,BangPatterns #-}

{-# OPTIONS_GHC -Wno-overflowed-literals #-}

module Search.MakeMove where

import Types
import Util.Fen
import Util.Utils
import Data.Bits
import Util.Bitboards
import Search.MoveConstants

removePieceFromBitboard :: Square -> Bitboard -> Bitboard
removePieceFromBitboard !square = (.&.) (complement (bit square))

moveWhiteRookWhenCastling :: Square -> Square -> Bitboard -> Bitboard -> Bitboard
moveWhiteRookWhenCastling !from !to !kingBoard !rookBoard
  | not (testBit kingBoard e1Bit) = rookBoard
  | from == e1Bit && to == g1Bit = movePieceWithinBitboard h1Bit f1Bit rookBoard
  | from == e1Bit && to == c1Bit = movePieceWithinBitboard a1Bit d1Bit rookBoard
  | otherwise = rookBoard

moveBlackRookWhenCastling :: Square -> Square -> Bitboard -> Bitboard -> Bitboard
moveBlackRookWhenCastling !from !to !kingBoard !rookBoard
  | not (testBit kingBoard e8Bit) = rookBoard
  | from == e8Bit && to == g8Bit = movePieceWithinBitboard h8Bit f8Bit rookBoard
  | from == e8Bit && to == c8Bit = movePieceWithinBitboard a8Bit d8Bit rookBoard
  | otherwise = rookBoard

enPassantCapturedPieceSquare :: Square -> Square
enPassantCapturedPieceSquare !enPassantSquare
  | enPassantSquare < 24 = enPassantSquare + 8
  | otherwise = enPassantSquare - 8

removePawnWhenEnPassant :: Bitboard -> Bitboard -> Square -> Square -> Bitboard
removePawnWhenEnPassant !attackerBb !defenderBb !to !enPassantSquare
  | enPassantSquare == to && testBit attackerBb to = removePieceFromBitboard (enPassantCapturedPieceSquare to) defenderBb
  | otherwise = defenderBb

removePawnIfPromotion :: Bitboard -> Bitboard
removePawnIfPromotion !bb = bb .&. 0b0000000011111111111111111111111111111111111111111111111100000000

isPromotionSquare :: Square -> Bool
isPromotionSquare !sq = (bit sq .&. promotionSquares) /= 0

createIfPromotion :: Bool -> Bitboard -> Bitboard -> Square -> Square -> Bitboard
createIfPromotion !isPromotionPiece !pawnBitboard !pieceBitboard !fromSquare !toSquare
  | isPromotionPiece && isPromotionSquare toSquare && testBit pawnBitboard fromSquare = pieceBitboard .|. bit toSquare
  | otherwise = pieceBitboard

movePieceWithinBitboard :: Square -> Square -> Bitboard -> Bitboard
movePieceWithinBitboard !from !to !bb
  | testBit bb from = (.|.) (clearBit bb from) (bit to)
  | otherwise = if testBit bb to then clearBit bb to else bb

makeMove :: Position -> Move -> Position
makeMove !position !move =
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
        , allPiecesBitboard = wp .|. bp .|. wn .|. bn .|. wb .|. bb .|. wr .|. br .|. wq .|. bq .|. wk .|. bk
        , whitePiecesBitboard = wp .|. wn .|. wr .|. wk .|. wq .|. wb
        , blackPiecesBitboard = bp .|. bn .|. br .|. bk .|. bq .|. bb
        , mover = if m == White then Black else White
        , enPassantSquare = if m == White
                                then if to - from == 16 && testBit (whitePawnBitboard position) from then from + 8 else enPassantNotAvailable
                                else if from - to == 16 && testBit (blackPawnBitboard position) from then from - 8 else enPassantNotAvailable
        , whiteKingCastleAvailable = whiteKingCastleAvailable position && notElem from [e1Bit,h1Bit] && to /= h1Bit
        , whiteQueenCastleAvailable = whiteQueenCastleAvailable position && notElem from [a1Bit,e1Bit] && to /= a1Bit
        , blackKingCastleAvailable = blackKingCastleAvailable position && notElem from [e8Bit,h8Bit] && to /= h8Bit
        , blackQueenCastleAvailable = blackQueenCastleAvailable position && notElem from [a8Bit,e8Bit] && to /= a8Bit
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



