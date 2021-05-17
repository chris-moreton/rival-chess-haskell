module Util.MagicMovesRook where

import Alias

import Util.MagicMovesRook1
import Util.MagicMovesRook2
import Util.MagicMovesRook3
import Util.MagicMovesRook4

magicMovesRook :: Int -> Int -> Bitboard
magicMovesRook square index
    | x == 0 = magicMovesRook1 square index
    | x == 1 = magicMovesRook2 square index
    | x == 2 = magicMovesRook3 square index
    | x == 3 = magicMovesRook4 square index
    | otherwise = 0
    where x = square `div` 16
