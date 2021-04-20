{-# LANGUAGE BinaryLiterals,NegativeLiterals #-}

module Search.MakeMove where

import Types
import Util.Fen
import Util.Utils
import Data.Bits
import Util.Bitboards

movePieceWithinBitboard :: Square -> Square -> Bitboard -> Bitboard
movePieceWithinBitboard from to bb
  | (.&.) bb (bit from) /= 0 = (.|.) ((.&.) bb (complement (bit from))) (bit to)
  | otherwise = bb

removePieceFromBitboard :: Square -> Bitboard -> Bitboard
removePieceFromBitboard square = (.&.) (complement (bit square))

moveWhiteRookWhenCastling :: Square -> Square -> Bitboard -> Bitboard -> Bitboard
moveWhiteRookWhenCastling from to kingBoard rookBoard
  | (.&.) kingBoard (1 `shiftL` bitRefFromAlgebraicSquareRef "e1") == 0 = rookBoard
  | from == bitRefFromAlgebraicSquareRef "e1" && to == bitRefFromAlgebraicSquareRef "g1" = movePieceWithinBitboard (bitRefFromAlgebraicSquareRef "h1") (bitRefFromAlgebraicSquareRef "f1") rookBoard
  | from == bitRefFromAlgebraicSquareRef "e1" && to == bitRefFromAlgebraicSquareRef "c1" = movePieceWithinBitboard (bitRefFromAlgebraicSquareRef "a1") (bitRefFromAlgebraicSquareRef "d1") rookBoard
  | otherwise = rookBoard

moveBlackRookWhenCastling :: Square -> Square -> Bitboard -> Bitboard -> Bitboard
moveBlackRookWhenCastling from to kingBoard rookBoard
  | (.&.) kingBoard (1 `shiftL` bitRefFromAlgebraicSquareRef "e8") == 0 = rookBoard
  | from == bitRefFromAlgebraicSquareRef "e8" && to == bitRefFromAlgebraicSquareRef "g8" = movePieceWithinBitboard (bitRefFromAlgebraicSquareRef "h8") (bitRefFromAlgebraicSquareRef "f8") rookBoard
  | from == bitRefFromAlgebraicSquareRef "e8" && to == bitRefFromAlgebraicSquareRef "c8" = movePieceWithinBitboard (bitRefFromAlgebraicSquareRef "a8") (bitRefFromAlgebraicSquareRef "d8") rookBoard
  | otherwise = rookBoard

enPassantCapturedPieceSquare :: Square -> Square
enPassantCapturedPieceSquare enPassantSquare
  | enPassantSquare < 24 = enPassantSquare + 8
  | otherwise = enPassantSquare - 8

removePawnWhenEnPassant :: Bitboard -> Square -> Square -> Bitboard
removePawnWhenEnPassant bb to enPassantSquare
  | enPassantSquare == to = removePieceFromBitboard (enPassantCapturedPieceSquare to) bb
  | otherwise = bb

removePawnIfPromotion :: Bitboard -> Bitboard
removePawnIfPromotion bb = bb .&. 0b0000000011111111111111111111111111111111111111111111111100000000

isPromotionSquare :: Square -> Bool
isPromotionSquare sq = (bit sq .&. promotionSquares) /= 0

createIfPromotion :: Bool -> Bitboard -> Bitboard -> Square -> Square -> Bitboard
createIfPromotion isPromotionPiece pawnBitboard pieceBitboard fromSquare toSquare
  | isPromotionPiece && isPromotionSquare toSquare && bit fromSquare .&. pawnBitboard /= 0 = pieceBitboard .|. bit toSquare
  | otherwise = pieceBitboard

makeMove :: Position -> Move -> Position
makeMove position move = makeSimpleMove position (fromSquarePart move) (toSquarePart move) (promotionPieceFromMove move)

makeSimpleMove :: Position -> Square -> Square -> Piece -> Position
makeSimpleMove position from to promotionPiece = do
  let m = mover position
  let bb = positionBitboards position
  let newWhitePawnBitboard = movePieceWithinBitboard from to (removePieceFromBitboard to (whitePawnBitboard bb))
  let newBlackPawnBitboard = movePieceWithinBitboard from to (removePieceFromBitboard to (blackPawnBitboard bb))
  let isCapture = (.&.) (bit to) (allPiecesBitboard position) /= 0 || to == enPassantSquare position
  let isPawnMove = newWhitePawnBitboard /= whitePawnBitboard bb || newBlackPawnBitboard /= blackPawnBitboard bb
  Position {
       positionBitboards = PieceBitboards {
            whitePawnBitboard = removePawnIfPromotion (removePawnWhenEnPassant newWhitePawnBitboard to (enPassantSquare position))
          , blackPawnBitboard = removePawnIfPromotion (removePawnWhenEnPassant newBlackPawnBitboard to (enPassantSquare position))
          , whiteKnightBitboard = createIfPromotion (promotionPiece == Knight) (whitePawnBitboard bb) (movePieceWithinBitboard from to (removePieceFromBitboard to (whiteKnightBitboard bb))) from to
          , blackKnightBitboard = createIfPromotion (promotionPiece == Knight) (blackPawnBitboard bb) (movePieceWithinBitboard from to (removePieceFromBitboard to (blackKnightBitboard bb))) from to
          , whiteBishopBitboard = createIfPromotion (promotionPiece == Bishop) (whitePawnBitboard bb) (movePieceWithinBitboard from to (removePieceFromBitboard to (whiteBishopBitboard bb))) from to
          , blackBishopBitboard = createIfPromotion (promotionPiece == Bishop) (blackPawnBitboard bb) (movePieceWithinBitboard from to (removePieceFromBitboard to (blackBishopBitboard bb))) from to
          , whiteRookBitboard = createIfPromotion (promotionPiece == Rook) (whitePawnBitboard bb) (moveWhiteRookWhenCastling from to (whiteKingBitboard bb) (movePieceWithinBitboard from to (removePieceFromBitboard to (whiteRookBitboard bb)))) from to
          , blackRookBitboard = createIfPromotion (promotionPiece == Rook) (blackPawnBitboard bb) (moveBlackRookWhenCastling from to (blackKingBitboard bb) (movePieceWithinBitboard from to (removePieceFromBitboard to (blackRookBitboard bb)))) from to
          , whiteQueenBitboard = createIfPromotion (promotionPiece == Queen) (whitePawnBitboard bb) (movePieceWithinBitboard from to (removePieceFromBitboard to (whiteQueenBitboard bb))) from to
          , blackQueenBitboard = createIfPromotion (promotionPiece == Queen) (blackPawnBitboard bb) (movePieceWithinBitboard from to (removePieceFromBitboard to (blackQueenBitboard bb))) from to
          , whiteKingBitboard = movePieceWithinBitboard from to (removePieceFromBitboard to (whiteKingBitboard bb))
          , blackKingBitboard = movePieceWithinBitboard from to (removePieceFromBitboard to (blackKingBitboard bb))
       }
     , mover = if m == White then Black else White
     , enPassantSquare = if m == White
                            then if to - from == 16 && bit from .&. whitePawnBitboard bb /= 0 then from + 8 else -1
                            else if from - to == 16 && bit from .&. blackPawnBitboard bb /= 0 then from - 8 else -1
     , positionCastlePrivs = CastlePrivileges {
            whiteKingCastleAvailable = whiteKingCastleAvailable (positionCastlePrivs position) && notElem from (map bitRefFromAlgebraicSquareRef ["e1","h1"])
          , whiteQueenCastleAvailable = whiteQueenCastleAvailable (positionCastlePrivs position) && notElem from (map bitRefFromAlgebraicSquareRef ["a1","e1"])
          , blackKingCastleAvailable = blackKingCastleAvailable (positionCastlePrivs position) && notElem from (map bitRefFromAlgebraicSquareRef ["e8","h8"])
          , blackQueenCastleAvailable = blackQueenCastleAvailable (positionCastlePrivs position) && notElem from (map bitRefFromAlgebraicSquareRef ["a8","e8"])
       }
     , halfMoves = if isCapture || isPawnMove then 0 else halfMoves position + 1
     , moveNumber = (+) (moveNumber position) (if m == Black then 1 else 0)
   }

