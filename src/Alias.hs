module Alias where

import qualified Data.Vector.Unboxed as V

type Square = Int
type Bitboard = Int
type Move = Int
type MoveList = V.Vector Move
type Path = [Move] -- Keep Path as a list for now
type MagicFunc = (Square -> Int -> Bitboard)
