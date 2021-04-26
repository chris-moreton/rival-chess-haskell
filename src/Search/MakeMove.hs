{-# LANGUAGE BinaryLiterals,NegativeLiterals,StrictData,BangPatterns #-}

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
  | from == e1Bit && to == g1Bit && kingMoving = movePieceWithinBitboard h1Bit f1Bit rookBoard
  | from == e1Bit && to == c1Bit && kingMoving = movePieceWithinBitboard a1Bit d1Bit rookBoard
  | otherwise = rookBoard
  where kingMoving = (.&.) kingBoard (bit e1Bit) /= 0

moveBlackRookWhenCastling :: Square -> Square -> Bitboard -> Bitboard -> Bitboard
moveBlackRookWhenCastling !from !to !kingBoard !rookBoard
  | (.&.) kingBoard (bit e8Bit) == 0 = rookBoard
  | from == e8Bit && to == g8Bit = movePieceWithinBitboard h8Bit f8Bit rookBoard
  | from == e8Bit && to == c8Bit = movePieceWithinBitboard a8Bit d8Bit rookBoard
  | otherwise = rookBoard

enPassantCapturedPieceSquare :: Square -> Square
enPassantCapturedPieceSquare !enPassantSquare
  | enPassantSquare < 24 = enPassantSquare + 8
  | otherwise = enPassantSquare - 8

removePawnWhenEnPassant :: Bitboard -> Bitboard -> Square -> Square -> Bitboard
removePawnWhenEnPassant !attackerBb !defenderBb !to !enPassantSquare
  | enPassantSquare == to && attackerBb .&. bit to /= 0 = removePieceFromBitboard (enPassantCapturedPieceSquare to) defenderBb
  | otherwise = defenderBb

removePawnIfPromotion :: Bitboard -> Bitboard
removePawnIfPromotion !bb = bb .&. 0b0000000011111111111111111111111111111111111111111111111100000000

isPromotionSquare :: Square -> Bool
isPromotionSquare !sq = (bit sq .&. promotionSquares) /= 0

createIfPromotion :: Bool -> Bitboard -> Bitboard -> Square -> Square -> Bitboard
createIfPromotion !isPromotionPiece !pawnBitboard !pieceBitboard !fromSquare !toSquare
  | isPromotionPiece && isPromotionSquare toSquare && bit fromSquare .&. pawnBitboard /= 0 = pieceBitboard .|. bit toSquare
  | otherwise = pieceBitboard

movePieceWithinBitboard :: Square -> Square -> Bitboard -> Bitboard
movePieceWithinBitboard !from !to !bb
  | (.&.) bb (bit from) /= 0 = (.|.) (clearBit (clearBit bb to) from) (bit to)
  | otherwise = clearBit bb to

makeMove :: Position -> Move -> Position
makeMove !position !move =
    Position {
        positionBitboards = PieceBitboards {
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
        }
        , mover = if m == White then Black else White
        , enPassantSquare = if m == White
                                then if to - from == 16 && bit from .&. whitePawnBitboard bb /= 0 then from + 8 else enPassantNotAvailable
                                else if from - to == 16 && bit from .&. blackPawnBitboard bb /= 0 then from - 8 else enPassantNotAvailable
        , positionCastlePrivs = CastlePrivileges {
              whiteKingCastleAvailable = whiteKingCastleAvailable (positionCastlePrivs position) && notElem from [e1Bit,h1Bit] && to /= h1Bit
            , whiteQueenCastleAvailable = whiteQueenCastleAvailable (positionCastlePrivs position) && notElem from [a1Bit,e1Bit] && to /= a1Bit
            , blackKingCastleAvailable = blackKingCastleAvailable (positionCastlePrivs position) && notElem from [e8Bit,h8Bit] && to /= h8Bit
            , blackQueenCastleAvailable = blackQueenCastleAvailable (positionCastlePrivs position) && notElem from [a8Bit,e8Bit] && to /= a8Bit
        }
        , halfMoves = if ((.&.) (bit to) (allPiecesBitboard position) /= 0) || isPawnMove then 0 else halfMoves position + 1
        , moveNumber = (+) (moveNumber position) (if m == Black then 1 else 0)
    }
    where from = fromSquarePart move
          to = toSquarePart move
          promotionPiece = promotionPieceFromMove move
          m = mover position
          bb = positionBitboards position
          newWhitePawnBitboard = movePieceWithinBitboard from to (removePieceFromBitboard to (whitePawnBitboard bb))
          newBlackPawnBitboard = movePieceWithinBitboard from to (removePieceFromBitboard to (blackPawnBitboard bb))
          isPawnMove = newWhitePawnBitboard /= whitePawnBitboard bb || newBlackPawnBitboard /= blackPawnBitboard bb



