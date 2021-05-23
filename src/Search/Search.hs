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
search position moveZero 0 low high endTime _ = return (moveZero,quiesce position low high)
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
    | m == White = testBit (blackPiecesBitboard position) t || e == t
    | otherwise = testBit (whitePiecesBitboard position) t || e == t
    where m = mover position
          t = toSquarePart move
          e = enPassantSquare position

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
    let whiteScore = (material position White) - (material position Black)
    if mover position == White then whiteScore else -whiteScore

quiesce :: Position -> Int -> Int -> Int
quiesce position low high = quiesceRecur position low high 0

quiesceRecur :: Position -> Int -> Int -> Int -> Int
quiesceRecur position _ _ 20 = evaluate position
quiesceRecur position low high depth = do
    let eval = evaluate position
    let newLow = max eval low
    let qp = quiescePositions position
    let l = length qp
    if not (null qp) && l <= 5
        then do
            let notInCheckPositions = filter (\(p,m) -> not (isCheck p (mover position))) qp
            if not (null notInCheckPositions)
                then if isCheck position (mover position) then (-9000)-depth else 0
                else highestQuiesceMove notInCheckPositions newLow high depth newLow
        else eval

highestQuiesceMove :: [(Position,Move)] -> Int -> Int -> Int -> Int -> Int
highestQuiesceMove [] _ _ _ best = best
highestQuiesceMove notInCheckPositions low high depth best = do
    let thisP = head notInCheckPositions
    let negatedScore = -(quiesceRecur (fst thisP) (-high) (-low) (depth-1))
    if negatedScore >= high
        then negatedScore
        else do
            if negatedScore > low
                then highestQuiesceMove (tail notInCheckPositions) negatedScore high depth negatedScore
                else highestQuiesceMove (tail notInCheckPositions) low high depth best
