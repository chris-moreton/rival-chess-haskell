module Search.MakeMove where

import Types
import Util.Fen
import Util.Utils
import Data.Bits

movePieceWithinBitboard :: Square -> Square -> Bitboard -> Bitboard
movePieceWithinBitboard from to bb
  | (.&.) bb (1 `shiftL` from) /= 0 = (.|.) ((.&.) bb (complement (1 `shiftL` from))) (1 `shiftL` to)
  | otherwise = bb

removePieceFromBitboard :: Square -> Bitboard -> Bitboard
removePieceFromBitboard square = (.&.) (complement (1 `shiftL` square))

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

makeMove :: Position -> Move -> Position
makeMove position move = do
  let fromSquare = fromSquarePart move
  let toSquare = toSquarePart move
  makeSimpleMove position fromSquare toSquare

makeSimpleMove :: Position -> Square -> Square -> Position
makeSimpleMove position from to = do
  let m = mover position
  let bb = positionBitboards position
  Position {
       positionBitboards = PieceBitboards {
            whitePawnBitboard =  movePieceWithinBitboard from to (removePieceFromBitboard to (whitePawnBitboard bb))
          , blackPawnBitboard = movePieceWithinBitboard from to (removePieceFromBitboard to (blackPawnBitboard bb))
          , whiteKnightBitboard = movePieceWithinBitboard from to (removePieceFromBitboard to (whiteKnightBitboard bb))
          , blackKnightBitboard = movePieceWithinBitboard from to (removePieceFromBitboard to (blackKnightBitboard bb))
          , whiteBishopBitboard = movePieceWithinBitboard from to (removePieceFromBitboard to (whiteBishopBitboard bb))
          , blackBishopBitboard = movePieceWithinBitboard from to (removePieceFromBitboard to (blackBishopBitboard bb))
          , whiteRookBitboard = moveWhiteRookWhenCastling from to (whiteKingBitboard bb) (movePieceWithinBitboard from to (removePieceFromBitboard to (whiteRookBitboard bb)))
          , blackRookBitboard = moveBlackRookWhenCastling from to (blackKingBitboard bb) (movePieceWithinBitboard from to (removePieceFromBitboard to (blackRookBitboard bb)))
          , whiteQueenBitboard = movePieceWithinBitboard from to (removePieceFromBitboard to (whiteQueenBitboard bb))
          , blackQueenBitboard = movePieceWithinBitboard from to (removePieceFromBitboard to (blackQueenBitboard bb))
          , whiteKingBitboard = movePieceWithinBitboard from to (removePieceFromBitboard to (whiteKingBitboard bb))
          , blackKingBitboard = movePieceWithinBitboard from to (removePieceFromBitboard to (blackKingBitboard bb))
       }
     , mover = if m == White then Black else White
     , enPassantSquare = -1
     , positionCastlePrivs = CastlePrivileges {
            whiteKingCastleAvailable = whiteKingCastleAvailable (positionCastlePrivs position) && notElem from (map bitRefFromAlgebraicSquareRef ["e1","h1"])
          , whiteQueenCastleAvailable = whiteQueenCastleAvailable (positionCastlePrivs position) && notElem from (map bitRefFromAlgebraicSquareRef ["a1","e1"])
          , blackKingCastleAvailable = blackKingCastleAvailable (positionCastlePrivs position) && notElem from (map bitRefFromAlgebraicSquareRef ["e8","h8"])
          , blackQueenCastleAvailable = blackQueenCastleAvailable (positionCastlePrivs position) && notElem from (map bitRefFromAlgebraicSquareRef ["a8","e8"])
       }
     , halfMoves = halfMoves position
     , moveNumber = (+) (moveNumber position) (if m == Black then 1 else 0)
   }

