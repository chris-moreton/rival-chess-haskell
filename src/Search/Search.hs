module Search.Search where

import Types ( Position (Position),mover, bitboardForColour, Piece (Pawn, Bishop, Knight, Rook, Queen), Mover (White,Black) )
import Alias ( Move )
import Search.MoveGenerator (moves,isCheck)
import Util.Utils ( timeMillis )
import Text.Printf
import Search.MakeMove ( makeMove )
import Data.Bits ( Bits(popCount) )
import Control.Monad

searchZero :: Position -> Int -> Int -> IO (Move,Int)
searchZero position depth endTime = do
    let newPositions = map (\move -> (makeMove position move,move)) (moves position)
    let notInCheckPositions = filter (\(p,m) -> not (isCheck p (mover position))) newPositions
    evaluatedMoves <- mapM (\(p,m) -> search p m depth endTime) notInCheckPositions
    let negatedMoves = map (\(m,i) -> (m,-i)) evaluatedMoves
    let highestRatedMove = foldr1 (\(m,s) (m',s') -> if s >= s' then (m,s) else (m',s')) negatedMoves
    return highestRatedMove

search :: Position -> Move -> Int -> Int -> IO (Move,Int)
search position moveZero 0 endTime = return (moveZero,evaluate position)
search position moveZero depth endTime = do
    t <- timeMillis 
    if t > endTime then return (moveZero,-9999) else do
        let newPositions = map (\move -> (makeMove position move,move)) (moves position)
        let notInCheckPositions = filter (\(p,m) -> not (isCheck p (mover position))) newPositions
        if null notInCheckPositions
            then return (moveZero,9999)
            else do
                evaluatedMoves <- mapM (\(p,m) -> search p moveZero (depth-1) endTime) notInCheckPositions
                let negatedMoves = map (\(m,i) -> (m,-i)) evaluatedMoves
                let highestRatedMove = foldr1 (\(m,s) (m',s') -> if s >= s' then (m,s) else (m',s')) negatedMoves
                return highestRatedMove

material :: Position -> Mover -> Int 
material position m = popCount (bitboardForColour position m Pawn) * 100 +
                      popCount (bitboardForColour position m Bishop) * 350 +
                      popCount (bitboardForColour position m Knight) * 350 +
                      popCount (bitboardForColour position m Rook) * 500 +
                      popCount (bitboardForColour position m Queen) * 900

evaluate :: Position -> Int
evaluate position = do
    let whiteScore = material position White - material position Black 
    if mover position == White then whiteScore else -whiteScore


