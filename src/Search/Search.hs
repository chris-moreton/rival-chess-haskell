module Search.Search where

import Types ( Position (Position),mover, bitboardForColour, Piece (Pawn, Bishop, Knight, Rook, Queen) )
import Alias ( Move )
import Search.MoveGenerator (moves,isCheck)
import Util.Utils
import Search.MakeMove
import Data.Bits

search :: Position -> Int -> IO Move
search position endTime = do
    let newPositions = map (\move -> (makeMove position move,move)) (moves position)
    let notInCheckPositions = filter (\(p,m) -> not (isCheck p (mover position))) newPositions
    let evaluatedMoves = map (\(p,m) -> (m,evaluate p)) notInCheckPositions
    let move = fst (foldr1 (\(m,s) (m',s') -> if s <= s' then (m,s) else (m',s')) evaluatedMoves)
    t <- timeMillis
    if t > endTime then return move else search position endTime

evaluate :: Position -> Int 
evaluate position = do
    let m = mover position
    popCount (bitboardForColour position m Pawn) * 100 +
        popCount (bitboardForColour position m Bishop) * 350 +
        popCount (bitboardForColour position m Knight) * 350 +
        popCount (bitboardForColour position m Rook) * 500 +
        popCount (bitboardForColour position m Queen) * 900
    
