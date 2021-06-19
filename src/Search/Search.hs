{-# LANGUAGE BinaryLiterals #-}

module Search.Search where

import Types
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
import Data.Maybe ( isJust, fromJust )
import State.State
import qualified Data.HashTable.IO as H
import Util.Zobrist
import Search.Evaluate

canLeadToDrawByRepetition :: Position -> [Position] -> Bool
canLeadToDrawByRepetition p ps
    | p `elem` ps = True
    | or ([makeMove p m `elem` ps | m <- moves p]) = True
    | otherwise = False

startSearch :: [Position] -> Int -> Int -> SearchState -> IO MoveScore
startSearch (position:positions) maxDepth endTime c = do
    let theseMoves = sortMoves position (moves position)
    let newPositions = map (\move -> (makeMove position move,move)) theseMoves
    let notInCheckPositions = filter (\(p,m) -> not (isCheck p (mover position))) newPositions
    iterativeDeepening (position:positions) 1 maxDepth endTime (mkMs (snd (head notInCheckPositions),-100000)) c

mkMs :: (Move,Int) -> MoveScore
mkMs mi = MoveScore { msMove=fst mi, msScore=snd mi, msBound=Exact }

iterativeDeepening :: [Position] -> Int -> Int -> Int -> MoveScore -> SearchState -> IO MoveScore
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

bestMoveFirst :: Position -> MoveScore -> [(Position,Move)]
bestMoveFirst position best = do
    let movesWithoutBest = sortMoves position (filter (\m -> m /= msScore best) (moves position))
    let newPositionsWithoutBest = map (\move -> (makeMove position move,move)) movesWithoutBest
    let bestPosition = (makeMove position (msMove best),msMove best)
    let notInCheckPositions = filter (\(p,m) -> not (isCheck p (mover position))) newPositionsWithoutBest
    bestPosition : notInCheckPositions

searchZero :: [Position] -> Int -> Int -> MoveScore -> SearchState -> IO MoveScore
searchZero positions depth endTime rootBest c = do
    let position = head positions
    let positionsWithBestFirst = bestMoveFirst position rootBest
    highestRatedMoveZero (bestMoveFirst position rootBest) positions (-100000) 100000 depth endTime (mkMs (snd (head positionsWithBestFirst),-100000)) rootBest c

highestRatedMoveZero :: [(Position,Move)] -> [Position] -> Int -> Int -> Int -> Int -> MoveScore -> MoveScore -> SearchState -> IO MoveScore
highestRatedMoveZero [] _ _ _ _ _ best _ _ = return best
highestRatedMoveZero (thisP:ps) positions low high depth endTime best rootBest c = do
   t <- timeMillis
   if t > endTime
       then return best
       else do
            searchResult <- uncurry search thisP depth (-high) (-low) endTime rootBest c
            let ms = if canLeadToDrawByRepetition (fst thisP) positions
                then mkMs (snd thisP,1)
                else searchResult
            let negatedScore = -(msScore ms)
            if negatedScore > low
                then highestRatedMoveZero ps positions negatedScore high depth endTime MoveScore { msMove=snd thisP,msScore=negatedScore,msBound=Exact } rootBest c
                else highestRatedMoveZero ps positions low high depth endTime best rootBest c

goodHashEntry :: Int -> Maybe HashEntry -> Bool
goodHashEntry depth he = do
    case he of
        Just x -> False && height x >= depth && bound x == Exact
        _      -> False

search :: Position -> Move -> Int -> Int -> Int -> Int -> MoveScore -> SearchState -> IO MoveScore
search position moveZero 0 low high endTime _ c = do
    q <- quiesce position low high c
    return (mkMs (moveZero,q))
search position moveZero depth low high endTime rootBest c = do
    let hpos = hashIndex position
    hentry <- H.lookup (h c) hpos
    if goodHashEntry depth hentry
        then do
            incCounter 100000000 c
            return (mkMs (move (fromJust hentry), score (fromJust hentry)))
        else do
            incCounter 1 c
            if halfMoves position == 50
                then return (mkMs (moveZero, 0))
                else do
                    t <- timeMillis
                    if t > endTime then return rootBest else do
                        let notInCheckPositions = filter (\(p,m) -> not (isCheck p (mover position))) (newPositions position)
                        if null notInCheckPositions
                            then return (mkMs (moveZero, if isCheck position (mover position) then (-9000)-depth else 0))
                            else do
                                hrm <- highestRatedMove notInCheckPositions moveZero low high depth endTime 
                                            MoveScore { msMove=snd (head notInCheckPositions), msScore=low, msBound=Upper } rootBest c
                                updateHashTable hpos HashEntry { score=msScore hrm, move=msMove hrm, height=depth, bound=msBound hrm } c
                                return hrm

highestRatedMove :: [(Position,Move)] -> Move -> Int -> Int -> Int -> Int -> MoveScore -> MoveScore -> SearchState -> IO MoveScore
highestRatedMove [] _ _ _ _ _ best _ _ = return best
highestRatedMove notInCheckPositions moveZero low high depth endTime best rootBest c = do
    let thisP = head notInCheckPositions
    ms <- search (fst thisP) moveZero (depth-1) (-high) (-low) endTime rootBest c
    let negatedScore = -(msScore ms)
    if negatedScore >= high
        then return ms { msScore=negatedScore, msBound=Lower }
        else do
            if negatedScore > low
                then highestRatedMove (tail notInCheckPositions) moveZero negatedScore high depth endTime ms { msScore=negatedScore, msBound=Exact } rootBest c
                else highestRatedMove (tail notInCheckPositions) moveZero low high depth endTime best rootBest c

newPositions :: Position -> [(Position,Move)]
newPositions position = map (\move -> (makeMove position move,move)) (moves position)

quiescePositions :: Position -> [(Position,Move)]
quiescePositions position = do
    let m = moves position
    let captures = filter (isCapture position) m
    map (\m -> (makeMove position m,m)) captures

quiesce :: Position -> Int -> Int -> SearchState -> IO Int
quiesce position low high = quiesceRecur position low high 0

quiesceRecur :: Position -> Int -> Int -> Int -> SearchState -> IO Int
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

highestQuiesceMove :: [(Position,Move)] -> Int -> Int -> Int -> Int -> SearchState -> IO Int
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
