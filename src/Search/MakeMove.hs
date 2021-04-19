module Search.MakeMove where

import Types
import Util.Fen
import Util.Utils
import Data.Bits

-- This is the section I am currently working on - it makes only simple from-to moves at the moment - no enpassants, castles or promotions

movePieceWithinBitboard :: Square -> Square -> Bitboard -> Bitboard
movePieceWithinBitboard from to bb
  | (.&.) bb (1 `shiftL` from) /= 0 = (.|.) ((.&.) bb (complement (1 `shiftL` from))) (1 `shiftL` to)
  | otherwise = bb

removePieceFromBitboard :: Square -> Bitboard -> Bitboard
removePieceFromBitboard square = (.&.) (complement (1 `shiftL` square))

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
          , whiteRookBitboard = movePieceWithinBitboard from to (removePieceFromBitboard to (whiteRookBitboard bb))
          , blackRookBitboard = movePieceWithinBitboard from to (removePieceFromBitboard to (blackRookBitboard bb))
          , whiteQueenBitboard = movePieceWithinBitboard from to (removePieceFromBitboard to (whiteQueenBitboard bb))
          , blackQueenBitboard = movePieceWithinBitboard from to (removePieceFromBitboard to (blackQueenBitboard bb))
          , whiteKingBitboard = movePieceWithinBitboard from to (removePieceFromBitboard to (whiteKingBitboard bb))
          , blackKingBitboard = movePieceWithinBitboard from to (removePieceFromBitboard to (blackKingBitboard bb))
       }
     , mover = if m == White then Black else White
     , enPassantSquare = -1
     , positionCastlePrivs = positionCastlePrivs position
     , halfMoves = halfMoves position
     , moveNumber = (+) (moveNumber position) (if m == Black then 1 else 0)
   }

