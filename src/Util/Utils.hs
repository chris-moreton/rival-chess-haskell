{-# LANGUAGE BangPatterns,OverloadedStrings #-}

module Util.Utils where

import Data.List
import Types
import Alias
import Data.Bits
import Search.MoveConstants
import Control.Parallel

substring :: String -> Int -> Int -> String
substring text start end = take (end - start) (drop start text)

{-# INLINE fromSquareMask #-}
fromSquareMask :: Square -> Move
fromSquareMask !from = from `shiftL` 16

{-# INLINE fromSquarePart #-}
fromSquarePart :: Move -> Square
fromSquarePart !move = move `shiftR` 16

{-# INLINE toSquarePart #-}
toSquarePart :: Move -> Square
toSquarePart move = (.&.) move 63

{-# INLINE promotionPieceFromMove #-}
promotionPieceFromMove :: Move -> Piece
promotionPieceFromMove move
    | promotionPart == 0 = Pawn
    | promotionPart == promotionQueenMoveMask = Queen
    | promotionPart == promotionRookMoveMask = Rook
    | promotionPart == promotionBishopMoveMask = Bishop
    | promotionPart == promotionKnightMoveMask = Knight
    | otherwise = Pawn
    where promotionPart = promotionFullMoveMask .&. move

{-# INLINE opponent #-}
opponent :: Position -> Mover
opponent position = if mover position == White then Black else White

{-# INLINE switchSide #-}
switchSide :: Mover -> Mover
switchSide White = Black
switchSide Black = White
