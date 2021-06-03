{-# LANGUAGE BinaryLiterals #-}

module Search.Search where

import Types ( Position (..), mover, halfMoves, bitboardForColour, Piece (..), Mover (White,Black) )
import Alias ( Move, Bitboard, MoveList )
import Search.MoveGenerator (moves,isCheck)
import Util.Utils ( timeMillis, toSquarePart )
import Text.Printf ()
import Util.Fen ( algebraicMoveFromMove )
import Search.MakeMove ( makeMove )
import Data.Bits ( Bits(popCount), Bits(testBit), Bits(bit), (.|.), (.&.), clearBit, shiftL )
import Control.Monad ()
import System.Exit ()
import Data.Sort ( sortBy )
import State.State ( Counter, incCounter )
import Search.Evaluate ( scoreMove, evaluate, isCapture )

hashPosition :: Position -> Int
hashPosition p =
    (if mover p == White then 1238799 else 12389876) + (2 * enPassantSquare p) + (3 * whitePawnBitboard  p) +
    (4 * blackPawnBitboard p) + (5 * whiteKnightBitboard  p) + (6 * blackKnightBitboard p) +
    (7 * whiteBishopBitboard p) + (8 * blackBishopBitboard p) +
    (11 * whiteRookBitboard p) + (12 * blackRookBitboard p) +
    (13 * whiteQueenBitboard p) + (14 * blackQueenBitboard p) +
    (15 * whiteKingBitboard p) + (16 * blackKingBitboard p)

canLeadToDrawByRepetition :: Position -> [Position] -> Bool
canLeadToDrawByRepetition p ps
    | p `elem` ps = True
    | or ([makeMove p m `elem` ps | m <- moves p]) = True
    | otherwise = False

startSearch :: [Position] -> Int -> Int -> Counter -> IO (Move,Int)
startSearch (position:positions) maxDepth endTime c = do
    let theseMoves = sortMoves position (moves position)
    let newPositions = map (\move -> (makeMove position move,move)) theseMoves
    let notInCheckPositions = filter (\(p,m) -> not (isCheck p (mover position))) newPositions
    iterativeDeepening (position:positions) 1 maxDepth endTime (snd (head notInCheckPositions),-100000) c

iterativeDeepening :: [Position] -> Int -> Int -> Int -> (Move,Int) -> Counter -> IO (Move,Int)
iterativeDeepening positions depth maxDepth endTime rootBest c = do
    result <- searchZero positions depth endTime rootBest c
    t <- timeMillis
    if t > endTime || depth == maxDepth
        then return result
        else iterativeDeepening positions (depth+1) maxDepth endTime result c

sortMoves :: Position -> MoveList -> MoveList
sortMoves position moves = do
    let scoredMoves = map (\m -> m + (scoreMove position m `shiftL` 32)) moves
    map (0b0000000000000000000000000000000011111111111111111111111111111111 .&.) (sortBy (flip compare) scoredMoves)

bestMoveFirst :: Position -> (Move,Int) -> [(Position,Move)]
bestMoveFirst position best = do
    let movesWithoutBest = sortMoves position (filter (\m -> m /= snd best) (moves position))
    let newPositionsWithoutBest = map (\move -> (makeMove position move,move)) movesWithoutBest
    let bestPosition = (makeMove position (fst best),fst best)
    let notInCheckPositions = filter (\(p,m) -> not (isCheck p (mover position))) newPositionsWithoutBest
    bestPosition : notInCheckPositions

searchZero :: [Position] -> Int -> Int -> (Move,Int) -> Counter -> IO (Move,Int)
searchZero positions depth endTime rootBest c = do
    let position = head positions
    let positionsWithBestFirst = bestMoveFirst position rootBest
    highestRatedMoveZero (bestMoveFirst position rootBest) positions (-100000) 100000 depth endTime (snd (head positionsWithBestFirst),-100000) rootBest c

highestRatedMoveZero :: [(Position,Move)] -> [Position] -> Int -> Int -> Int -> Int -> (Move,Int) -> (Move,Int) -> Counter -> IO (Move,Int)
highestRatedMoveZero [] _ _ _ _ _ best _ _ = return best
highestRatedMoveZero (thisP:ps) positions low high depth endTime best rootBest c = do
   t <- timeMillis
   if t > endTime
       then return best
       else do
            searchResult <- uncurry search thisP depth (-high) (-low) endTime rootBest c
            let (m,s) = if canLeadToDrawByRepetition (fst thisP) positions
                then (snd thisP,1)
                else searchResult
            let negatedScore = -s
            if negatedScore > low
                then highestRatedMoveZero ps positions negatedScore high depth endTime (snd thisP,negatedScore) rootBest c
                else highestRatedMoveZero ps positions low high depth endTime best rootBest c

search :: Position -> Move -> Int -> Int -> Int -> Int -> (Move,Int) -> Counter -> IO (Move,Int)
search position moveZero 0 low high endTime _ c = do
    q <- quiesce position low high c
    return (moveZero,q)
search position moveZero depth low high endTime rootBest c = do
    incCounter 1 c
    if halfMoves position == 50
        then return (moveZero, 0)
        else do
            t <- timeMillis
            if t > endTime then return rootBest else do
                let notInCheckPositions = filter (\(p,m) -> not (isCheck p (mover position))) (newPositions position)
                if null notInCheckPositions
                    then return (moveZero, if isCheck position (mover position) then (-9000)-depth else 0)
                    else highestRatedMove notInCheckPositions moveZero low high depth endTime (snd (head notInCheckPositions),low) rootBest c

highestRatedMove :: [(Position,Move)] -> Move -> Int -> Int -> Int -> Int -> (Move,Int) -> (Move,Int) -> Counter -> IO (Move,Int)
highestRatedMove [] _ _ _ _ _ best _ _ = return best
highestRatedMove notInCheckPositions moveZero low high depth endTime best rootBest c = do
    let thisP = head notInCheckPositions
    (m,s) <- search (fst thisP) moveZero (depth-1) (-high) (-low) endTime rootBest c
    let negatedScore = -s
    if negatedScore >= high
        then return (m,negatedScore)
        else do
            if negatedScore > low
                then highestRatedMove (tail notInCheckPositions) moveZero negatedScore high depth endTime (m,negatedScore) rootBest c
                else highestRatedMove (tail notInCheckPositions) moveZero low high depth endTime best rootBest c

newPositions :: Position -> [(Position,Move)]
newPositions position = map (\move -> (makeMove position move,move)) (moves position)

quiescePositions :: Position -> [(Position,Move)]
quiescePositions position = do
    let m = moves position
    let captures = filter (isCapture position) m
    map (\m -> (makeMove position m,m)) captures

quiesce :: Position -> Int -> Int -> Counter -> IO Int
quiesce position low high = quiesceRecur position low high 0

quiesceRecur :: Position -> Int -> Int -> Int -> Counter -> IO Int
quiesceRecur position _ _ 10 c = do
    incCounter 1 c
    return (evaluate position)
quiesceRecur position low high depth c = do
    incCounter 1 c
    let eval = evaluate position
    let newLow = max eval low
    let qp = quiescePositions position
    let l = length qp
    if not (null qp)
        then do
            let notInCheckPositions = filter (\(p,m) -> not (isCheck p (mover position))) qp
            if null notInCheckPositions
                then return (if isCheck position (mover position) then (-9000)-depth else 0)
                else highestQuiesceMove notInCheckPositions newLow high depth newLow c
        else return newLow

highestQuiesceMove :: [(Position,Move)] -> Int -> Int -> Int -> Int -> Counter -> IO Int
highestQuiesceMove [] _ _ _ best c = return best
highestQuiesceMove notInCheckPositions low high depth best c = do
    let thisP = head notInCheckPositions
    score <- quiesceRecur (fst thisP) (-high) (-low) (depth+1) c
    let negatedScore = -score
    if negatedScore >= high
        then return negatedScore
        else do
            if negatedScore > low
                then highestQuiesceMove (tail notInCheckPositions) negatedScore high depth negatedScore c
                else highestQuiesceMove (tail notInCheckPositions) low high depth best c
