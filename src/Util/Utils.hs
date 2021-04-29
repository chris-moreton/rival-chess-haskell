{-# LANGUAGE OverloadedStrings #-}

module Util.Utils where

import Data.List
import Types
import Alias
import Data.Bits
import Search.MoveConstants
import GHC.Compact

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
    | promotionPart == 0 = Pawn
    | promotionPart == promotionQueenMoveMask = Queen
    | promotionPart == promotionRookMoveMask = Rook
    | promotionPart == promotionBishopMoveMask = Bishop
    | promotionPart == promotionKnightMoveMask = Knight
    | otherwise = Pawn
    where promotionPart = (.&.) promotionFullMoveMask move

opponent :: Position -> Mover
opponent position = if mover position == White then Black else White
