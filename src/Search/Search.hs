module Search.Search where

import Types ( Position (Position),mover, bitboardForColour, Piece (Pawn, Bishop, Knight, Rook, Queen) )
import Alias ( Move )
import Search.MoveGenerator (moves,isCheck)
import Util.Utils ( timeMillis )
import Search.MakeMove ( makeMove )
import Data.Bits ( Bits(popCount) )
import Control.Monad

searchZero :: Position -> Int -> IO (Move,Int)
searchZero position endTime = do
    let newPositions = map (\move -> (makeMove position move,move)) (moves position)
    let notInCheckPositions = filter (\(p,m) -> not (isCheck p (mover position))) newPositions
    evaluatedMoves <- mapM (\(p,m) -> search p m 3 endTime) notInCheckPositions
    let highestRatedMove = foldr1 (\(m,s) (m',s') -> if s <= s' then (m,s) else (m',s')) evaluatedMoves
    return highestRatedMove

search :: Position -> Move -> Int -> Int -> IO (Move,Int)
search position move depth endTime = do
    return (move,evaluate position)

evaluate :: Position -> Int
evaluate position = do
    let m = mover position
    popCount (bitboardForColour position m Pawn) * 100 +
        popCount (bitboardForColour position m Bishop) * 350 +
        popCount (bitboardForColour position m Knight) * 350 +
        popCount (bitboardForColour position m Rook) * 500 +
        popCount (bitboardForColour position m Queen) * 900

