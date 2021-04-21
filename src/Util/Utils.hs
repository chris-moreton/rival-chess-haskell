{-# LANGUAGE OverloadedStrings #-}

module Util.Utils where

import Data.List
import Types
import Data.Bits
import Search.MoveConstants

substring :: String -> Int -> Int -> String
substring text start end = take (end - start) (drop start text)

fromSquareMask :: Square -> Move
fromSquareMask sq = sq `shiftL` 16

fromSquarePart :: Move -> Square
fromSquarePart move = move `shiftR` 16

toSquarePart :: Move -> Square
toSquarePart move = (.&.) move 63

promotionPieceFromMove :: Move -> Piece
promotionPieceFromMove move
    | (.&.) promotionFullMoveMask move == promotionQueenMoveMask = Queen
    | (.&.) promotionFullMoveMask move == promotionRookMoveMask = Rook
    | (.&.) promotionFullMoveMask move == promotionBishopMoveMask = Bishop
    | (.&.) promotionFullMoveMask move == promotionKnightMoveMask = Knight
    | otherwise = Pawn

opponent :: Position -> Mover
opponent position = if mover position == White then Black else White
