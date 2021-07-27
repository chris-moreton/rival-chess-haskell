module Search.See.StaticExchangeEval where

import Evaluate.Evaluate
import Search.MakeMove (makeMove)
import Search.SearchHelper (kingCaptured)
import Types
import Alias ( Move, Square )
import Data.Bits ( Bits((.&.)) )

staticExchangeEvaluation :: Position -> Move -> Int
staticExchangeEvaluation position move =
    if kingCaptured newPosition
        then -100000
        else -(seeSearch newPosition toSquare capVal)
    where
        newPosition = makeMove position move
        toSquare = (.&.) 63 move
        capVal = -(pieceValue $ capturePiece position move)

        capturePiece :: Position -> Move -> Piece
        capturePiece position move
            | m == White && whitePawnBitboard position /= whitePawnBitboard newPosition = Pawn
            | m == Black && blackPawnBitboard position /= blackPawnBitboard newPosition = Pawn
            | m == White && whiteKnightBitboard position /= whiteKnightBitboard newPosition = Knight
            | m == Black && blackKnightBitboard position /= blackKnightBitboard newPosition = Knight
            | m == White && whiteBishopBitboard position /= whiteBishopBitboard newPosition = Bishop
            | m == Black && blackBishopBitboard position /= blackBishopBitboard newPosition = Bishop
            | m == White && whiteRookBitboard position /= whiteRookBitboard newPosition = Rook
            | m == Black && blackRookBitboard position /= blackRookBitboard newPosition = Rook
            | m == White && whiteQueenBitboard position /= whiteQueenBitboard newPosition = Queen
            | m == Black && blackQueenBitboard position /= blackQueenBitboard newPosition = Queen
            where 
                newPosition = makeMove position move
                m = mover newPosition

        seeSearch :: Position -> Square -> Int -> Int
        seeSearch position square balance = 100