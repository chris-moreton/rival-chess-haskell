{-# LANGUAGE StrictData,BangPatterns #-}

module Search.Perft where

import Types
import Search.MakeMove
import Search.MoveGenerator
import Control.DeepSeq
import Control.Parallel.Strategies

parFilter :: (NFData a) => (a -> Bool) -> [a] -> [a]
parFilter f = filter f
--parFilter f = withStrategy (parList rdeepseq) . filter f

perft :: Position -> Int -> Int
perft !position !depth =
    if depth == 0
        then length notInCheckPositions
        else sum (map (\x -> perft x (depth - 1)) notInCheckPositions)
    where !newPositions = map (makeMove position) (moves position)
          !notInCheckPositions = parFilter (\x -> not (isCheck x (mover position))) newPositions
