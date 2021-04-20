module Search.Perft where

import Types
import Search.MakeMove
import Search.MoveGenerator

perft :: Position -> Int -> Int
perft position depth = do
  let newPositions = map (makeMove position) (moves position)
  let notInCheckPositions = filter (\x -> not (isCheck x (mover position))) newPositions
  if depth == 0 then length notInCheckPositions else sum (map (\x -> perft x (depth - 1)) notInCheckPositions)
