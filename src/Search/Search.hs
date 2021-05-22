module Search.Search where

import Types ( Position (Position, whitePiecesBitboard, blackPiecesBitboard, enPassantSquare), mover, halfMoves, bitboardForColour, Piece (Pawn, Bishop, Knight, Rook, Queen), Mover (White,Black) )
import Alias ( Move, Bitboard )
import Search.MoveGenerator (moves,isCheck)
import Util.Utils ( timeMillis, toSquarePart )
import Text.Printf
import Search.MakeMove ( makeMove )
import Data.Bits ( Bits(popCount), Bits(testBit), Bits(bit) )
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
    let notInCheckPositions = filter (\(p,m) -> not (isCheck p (mover position))) (newPositions position)
    evaluatedMoves <- mapM (\(p,m) -> if canLeadToDrawByRepetition p positions then return (m,1) else search p m depth (-100000) 100000 endTime rootBest) notInCheckPositions
    let negatedMoves = map (\(m,i) -> (m,-i)) evaluatedMoves
    let highestRatedMove = foldr1 (\(m,s) (m',s') -> if s >= s' then (m,s) else (m',s')) negatedMoves
    return highestRatedMove

search :: Position -> Move -> Int -> Int -> Int -> Int -> (Move,Int) -> IO (Move,Int)
search position moveZero 0 _ _ endTime _ = return (moveZero,quiesce position)
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

isCapture :: Position -> Move -> Bool
isCapture position move
    | m == White = testBit (blackPiecesBitboard position) t || testBit e t
    | otherwise = testBit (whitePiecesBitboard position) t || testBit e t
    where m = mover position
          t = toSquarePart move
          e = bit (enPassantSquare position) :: Bitboard

quiescePositions :: Position -> [(Position,Move)]
quiescePositions position = do
    let m = moves position
    let captures = filter (isCapture position) m
    map (\m -> (makeMove position m,m)) captures

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

quiesce :: Position -> Int
quiesce position = quiesceRecur position 0

quiesceRecur :: Position -> Int -> Int
quiesceRecur position 15 = evaluate position
quiesceRecur position depth = do
    let eval = evaluate position
    let notInCheckPositions = filter (\(p,m) -> not (isCheck p (mover position))) (newPositions position)
    let evaluatedMoves = map (\(p,m) -> quiesceRecur p (depth+1)) notInCheckPositions
    let negatedMoves = map (\i -> -i) evaluatedMoves
    let highestRatedMove = foldr1 (\s s' -> if s >= s' then s else s') negatedMoves
    if highestRatedMove > eval then highestRatedMove else eval

