module Search.Search where

import Types ( Position (Position), mover, halfMoves, bitboardForColour, Piece (Pawn, Bishop, Knight, Rook, Queen), Mover (White,Black) )
import Alias ( Move )
import Search.MoveGenerator (moves,isCheck)
import Util.Utils ( timeMillis )
import Text.Printf
import Search.MakeMove ( makeMove )
import Data.Bits ( Bits(popCount) )
import Control.Monad

------------------------------------------------------
-- example of how the IDE simplified my newbie Haskell
------------------------------------------------------
-- any (\oldPos -> p == oldPos) positions
-- any (p ==) positions
-- p `elem` positions
------------------------------------------------------

canLeadToDrawByRepetition :: Position -> [Position] -> Bool
canLeadToDrawByRepetition p ps
    | p `elem` ps = True
    | or ([makeMove p m `elem` ps | m <- moves p]) = True
    | otherwise = False

startSearch :: [Position] -> Int -> Int -> IO (Move,Int)
startSearch positions maxDepth endTime = iterativeDeepening positions 1 maxDepth endTime (head (moves (head positions)),0)

iterativeDeepening :: [Position] -> Int -> Int -> Int -> (Move,Int) -> IO (Move,Int)
iterativeDeepening positions depth maxDepth endTime result = do
    result <- searchZero positions depth endTime result
    t <- timeMillis
    if t > endTime || depth == maxDepth
        then return result
        else iterativeDeepening positions (depth+1) maxDepth endTime result

searchZero :: [Position] -> Int -> Int -> (Move,Int) -> IO (Move,Int)
searchZero positions depth endTime rootBest = do
    let position = head positions
    let newPositions = map (\move -> (makeMove position move,move)) (moves position)
    let notInCheckPositions = filter (\(p,m) -> not (isCheck p (mover position))) newPositions
    highestRatedMoveZero notInCheckPositions positions (-100000) 100000 depth endTime (snd (head notInCheckPositions),-100000) rootBest

highestRatedMoveZero :: [(Position,Move)] -> [Position] -> Int -> Int -> Int -> Int -> (Move,Int) -> (Move,Int) -> IO (Move,Int)
highestRatedMoveZero [] _ _ _ _ _ best _ = return best
highestRatedMoveZero notInCheckPositions positions low high depth endTime best rootBest = do
    evaluatedMoves <- mapM (\(p,m) -> if canLeadToDrawByRepetition p positions then return (m,1) else search p m depth (-100000) 100000 endTime rootBest) notInCheckPositions
    let negatedMoves = map (\(m,i) -> (m,-i)) evaluatedMoves
    let highestRatedMove = foldr1 (\(m,s) (m',s') -> if s >= s' then (m,s) else (m',s')) negatedMoves
    return highestRatedMove

--    searchResult <- uncurry search thisP depth (-high) (-low) endTime rootBest
--    let (m,s) = if canLeadToDrawByRepetition (fst thisP) positions
--        then (snd thisP,1)
--        else searchResult
--    let negatedScore = -s
--    if negatedScore > low
--        then highestRatedMoveZero ps positions negatedScore high depth endTime (m,negatedScore) rootBest
--        else highestRatedMoveZero ps positions low high depth endTime best rootBest

search :: Position -> Move -> Int -> Int -> Int -> Int -> (Move,Int) -> IO (Move,Int)
search position moveZero 0 _ _ endTime _ = return (moveZero,evaluate position)
search position moveZero depth low high endTime rootBest = do
    if halfMoves position == 50 
        then return (moveZero, 0) 
        else do
            t <- timeMillis
            if t > endTime then return rootBest else do
                let notInCheckPositions = filter (\(p,m) -> not (isCheck p (mover position))) (newPositions position)
                if null notInCheckPositions
                    then return (moveZero, if isCheck position (mover position) then (-9000)-depth else 0)
                    else highestRatedMove notInCheckPositions moveZero low high depth endTime (snd (head notInCheckPositions),low) rootBest

highestRatedMove :: [(Position,Move)] -> Move -> Int -> Int -> Int -> Int -> (Move,Int) -> (Move,Int) -> IO (Move,Int)
highestRatedMove [] _ _ _ _ _ best _ = return best
highestRatedMove notInCheckPositions moveZero low high depth endTime best rootBest = do
    let thisP = head notInCheckPositions
    (m,s) <- search (fst thisP) moveZero (depth-1) (-high) (-low) endTime rootBest
    let negatedScore = -s
    if negatedScore >= high
        then return (m,negatedScore)
        else do
            if negatedScore > low
                then highestRatedMove (tail notInCheckPositions) moveZero negatedScore high depth endTime (m,negatedScore) rootBest
                else highestRatedMove (tail notInCheckPositions) moveZero low high depth endTime best rootBest

newPositions :: Position -> [(Position,Move)]
newPositions position = map (\move -> (makeMove position move,move)) (moves position)

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
