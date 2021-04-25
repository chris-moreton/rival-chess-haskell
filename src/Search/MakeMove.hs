{-# LANGUAGE BinaryLiterals,NegativeLiterals,StrictData,BangPatterns #-}

module Search.MakeMove where

import Types
import Util.Fen
import Util.Utils
import Data.Bits
import Util.Bitboards
import Search.MoveConstants

movePieceWithinBitboard :: Square -> Square -> Bitboard -> Bitboard
movePieceWithinBitboard !from !to !bb
  | testBit bb from = setBit (clearBit bb from) to
  | otherwise = bb

moveWhiteRookWhenCastling :: Square -> Square -> Bitboard -> Bitboard -> Bitboard
moveWhiteRookWhenCastling !from !to !kingBoard !rookBoard
  | from == e1Bit && to == g1Bit && kingMoving = movePieceWithinBitboard h1Bit f1Bit rookBoard
  | from == e1Bit && to == c1Bit && kingMoving = movePieceWithinBitboard a1Bit d1Bit rookBoard
  | otherwise = rookBoard
  where kingMoving = testBit kingBoard (bit e1Bit)

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
  | enPassantSquare == to && testBit attackerBb to = clearBit defenderBb (enPassantCapturedPieceSquare to)
  | otherwise = defenderBb

removePawnIfPromotion :: Bitboard -> Bitboard
removePawnIfPromotion !bb = bb .&. 0b0000000011111111111111111111111111111111111111111111111100000000

createIfPromotion :: Bool -> Bitboard -> Bitboard -> Square -> Square -> Bitboard
createIfPromotion !isPromotionPiece !pawnBitboard !pieceBitboard !fromSquare !toSquare
  | isPromotionPiece && testBit promotionSquares toSquare && testBit pawnBitboard fromSquare = setBit pieceBitboard toSquare
  | otherwise = pieceBitboard

makeMove :: Position -> Move -> Position
makeMove !position !move =
    Position {
        positionBitboards = PieceBitboards {
              whitePawnBitboard = removePawnIfPromotion (removePawnWhenEnPassant newBlackPawnBitboard newWhitePawnBitboard to (enPassantSquare position))
            , blackPawnBitboard = removePawnIfPromotion (removePawnWhenEnPassant newWhitePawnBitboard newBlackPawnBitboard to (enPassantSquare position))
            , whiteKnightBitboard = createIfPromotion (promotionPiece == Knight) (whitePawnBitboard bb) (movePieceWithinBitboard from to (clearBit (whiteKnightBitboard bb) to)) from to
            , blackKnightBitboard = createIfPromotion (promotionPiece == Knight) (blackPawnBitboard bb) (movePieceWithinBitboard from to (clearBit (blackKnightBitboard bb) to)) from to
            , whiteBishopBitboard = createIfPromotion (promotionPiece == Bishop) (whitePawnBitboard bb) (movePieceWithinBitboard from to (clearBit (whiteBishopBitboard bb) to)) from to
            , blackBishopBitboard = createIfPromotion (promotionPiece == Bishop) (blackPawnBitboard bb) (movePieceWithinBitboard from to (clearBit (blackBishopBitboard bb) to)) from to
            , whiteRookBitboard = createIfPromotion (promotionPiece == Rook) (whitePawnBitboard bb) (moveWhiteRookWhenCastling from to (whiteKingBitboard bb) (movePieceWithinBitboard from to (clearBit (whiteRookBitboard bb) to))) from to
            , blackRookBitboard = createIfPromotion (promotionPiece == Rook) (blackPawnBitboard bb) (moveBlackRookWhenCastling from to (blackKingBitboard bb) (movePieceWithinBitboard from to (clearBit (blackRookBitboard bb) to))) from to
            , whiteQueenBitboard = createIfPromotion (promotionPiece == Queen) (whitePawnBitboard bb) (movePieceWithinBitboard from to (clearBit (whiteQueenBitboard bb) to)) from to
            , blackQueenBitboard = createIfPromotion (promotionPiece == Queen) (blackPawnBitboard bb) (movePieceWithinBitboard from to (clearBit (blackQueenBitboard bb) to)) from to
            , whiteKingBitboard = movePieceWithinBitboard from to (clearBit (whiteKingBitboard bb) to)
            , blackKingBitboard = movePieceWithinBitboard from to (clearBit (blackKingBitboard bb) to)
        }
        , mover = opponent position
        , enPassantSquare = if mover position == White
                                then if to - from == 16 && bit from .&. whitePawnBitboard bb /= 0 then from + 8 else enPassantNotAvailable
                                else if from - to == 16 && bit from .&. blackPawnBitboard bb /= 0 then from - 8 else enPassantNotAvailable
        , positionCastlePrivs = CastlePrivileges {
              whiteKingCastleAvailable = whiteKingCastleAvailable (positionCastlePrivs position) && notElem from [e1Bit,h1Bit] && to /= h1Bit
            , whiteQueenCastleAvailable = whiteQueenCastleAvailable (positionCastlePrivs position) && notElem from [a1Bit,e1Bit] && to /= a1Bit
            , blackKingCastleAvailable = blackKingCastleAvailable (positionCastlePrivs position) && notElem from [e8Bit,h8Bit] && to /= h8Bit
            , blackQueenCastleAvailable = blackQueenCastleAvailable (positionCastlePrivs position) && notElem from [a8Bit,e8Bit] && to /= a8Bit
        }
        , halfMoves = if testBit (allPiecesBitboard position) to || isPawnMove then 0 else halfMoves position + 1
        , moveNumber = (+) (moveNumber position) (if mover position == Black then 1 else 0)
    }
    where from = fromSquarePart move
          to = toSquarePart move
          promotionPiece = promotionPieceFromMove move
          bb = positionBitboards position
          newWhitePawnBitboard = movePieceWithinBitboard from to (clearBit (whitePawnBitboard bb) to)
          newBlackPawnBitboard = movePieceWithinBitboard from to (clearBit (blackPawnBitboard bb) to)
          isPawnMove = newWhitePawnBitboard /= whitePawnBitboard bb || newBlackPawnBitboard /= blackPawnBitboard bb



