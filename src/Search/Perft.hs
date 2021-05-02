{-# LANGUAGE StrictData,BangPatterns #-}

module Search.Perft where

import Types
import Search.MakeMove
import Search.MoveGenerator

perft :: Position -> Int -> Int
perft !position !depth =
    if depth == 0
        then length notInCheckPositions
        else sum (map (\x -> perft x (depth - 1)) notInCheckPositions)
    where newPositions = map (makeMove position) (moves position)
          notInCheckPositions = filter (\x -> not (isCheck x (mover position))) newPositions
