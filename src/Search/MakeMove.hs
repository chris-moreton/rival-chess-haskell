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
  | otherwise = clearBit bb to

makeMove :: Position -> Move -> Position
makeMove !position !move =
    Position {
          whitePawnBitboard = removePawnIfPromotion (removePawnWhenEnPassant newBlackPawnBitboard newWhitePawnBitboard to (enPassantSquare position))
        , blackPawnBitboard = removePawnIfPromotion (removePawnWhenEnPassant newWhitePawnBitboard newBlackPawnBitboard to (enPassantSquare position))
        , whiteKnightBitboard = createIfPromotion (promotionPiece == Knight) (whitePawnBitboard bb) (movePieceWithinBitboard from to (whiteKnightBitboard bb)) from to
        , blackKnightBitboard = createIfPromotion (promotionPiece == Knight) (blackPawnBitboard bb) (movePieceWithinBitboard from to (blackKnightBitboard bb)) from to
        , whiteBishopBitboard = createIfPromotion (promotionPiece == Bishop) (whitePawnBitboard bb) (movePieceWithinBitboard from to (whiteBishopBitboard bb)) from to
        , blackBishopBitboard = createIfPromotion (promotionPiece == Bishop) (blackPawnBitboard bb) (movePieceWithinBitboard from to (blackBishopBitboard bb)) from to
        , whiteRookBitboard = createIfPromotion (promotionPiece == Rook) (whitePawnBitboard bb) (moveWhiteRookWhenCastling from to (whiteKingBitboard bb) (movePieceWithinBitboard from to (whiteRookBitboard bb))) from to
        , blackRookBitboard = createIfPromotion (promotionPiece == Rook) (blackPawnBitboard bb) (moveBlackRookWhenCastling from to (blackKingBitboard bb) (movePieceWithinBitboard from to (blackRookBitboard bb))) from to
        , whiteQueenBitboard = createIfPromotion (promotionPiece == Queen) (whitePawnBitboard bb) (movePieceWithinBitboard from to (whiteQueenBitboard bb)) from to
        , blackQueenBitboard = createIfPromotion (promotionPiece == Queen) (blackPawnBitboard bb) (movePieceWithinBitboard from to (blackQueenBitboard bb)) from to
        , whiteKingBitboard = movePieceWithinBitboard from to (whiteKingBitboard bb)
        , blackKingBitboard = movePieceWithinBitboard from to (blackKingBitboard bb)
        , mover = if m == White then Black else White
        , enPassantSquare = if m == White
                                then if to - from == 16 && testBit (whitePawnBitboard bb) from then from + 8 else enPassantNotAvailable
                                else if from - to == 16 && testBit (blackPawnBitboard bb) from then from - 8 else enPassantNotAvailable
        , positionCastlePrivs = if (from .|. to) .&. 0b1000100100000000000000000000000000000000000000000000000010001001 == 0 then positionCastlePrivs position else CastlePrivileges {
              whiteKingCastleAvailable = whiteKingCastleAvailable (positionCastlePrivs position) && notElem from [e1Bit,h1Bit] && to /= h1Bit
            , whiteQueenCastleAvailable = whiteQueenCastleAvailable (positionCastlePrivs position) && notElem from [a1Bit,e1Bit] && to /= a1Bit
            , blackKingCastleAvailable = blackKingCastleAvailable (positionCastlePrivs position) && notElem from [e8Bit,h8Bit] && to /= h8Bit
            , blackQueenCastleAvailable = blackQueenCastleAvailable (positionCastlePrivs position) && notElem from [a8Bit,e8Bit] && to /= a8Bit
        }
        , halfMoves = if testBit (allPiecesBitboard position) to || isPawnMove then 0 else halfMoves position + 1
        , moveNumber = (+) (moveNumber position) (if m == Black then 1 else 0)
    }
    where from = fromSquarePart move
          to = toSquarePart move
          promotionPiece = promotionPieceFromMove move
          m = mover position
          bb = position
          newWhitePawnBitboard = movePieceWithinBitboard from to (whitePawnBitboard bb)
          newBlackPawnBitboard = movePieceWithinBitboard from to (blackPawnBitboard bb)
          isPawnMove = newWhitePawnBitboard /= whitePawnBitboard bb || newBlackPawnBitboard /= blackPawnBitboard bb



