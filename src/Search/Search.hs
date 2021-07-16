{-# LANGUAGE BinaryLiterals #-}

module Search.Search where

import Types
    ( Bound(..),
      HashEntry(..),
      MoveScore(..),
      Position(halfMoves, mover) )
import Alias ( Move, Bitboard, MoveList, Path )
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
import State.State ( incNodes, updateHashTable, SearchState(..), calcHashIndex, setPv )
import qualified Data.HashTable.IO as H
import Util.Zobrist ( hashIndex, zobrist )
import Search.Evaluate ( evaluate, isCapture, scoreMove )
import Search.SearchHelper
import Search.Quiesce

startSearch :: [Position] -> Int -> Int -> SearchState -> IO MoveScore
startSearch (position:positions) maxDepth endTime searchState = do
    let theseMoves = sortMoves position 0 (moves position)
    let newPositions = map (\move -> (makeMove position move,move)) theseMoves
    let notInCheckPositions = filter (\(p,m) -> not (isCheck p (mover position))) newPositions
    let ms = mkMs (-100000, [snd (head notInCheckPositions)])
    result <- iterativeDeepening (position:positions) 1 maxDepth endTime ms searchState
    setPv (msPath result) searchState
    return result
    where
        iterativeDeepening :: [Position] -> Int -> Int -> Int -> MoveScore -> SearchState -> IO MoveScore
        iterativeDeepening positions depth maxDepth endTime rootBest searchState = do
            result <- searchZero positions depth endTime rootBest searchState
            t <- timeMillis
            if t > endTime || depth == maxDepth
                then return result
                else iterativeDeepening positions (depth+1) maxDepth endTime result searchState

searchZero :: [Position] -> Int -> Int -> MoveScore -> SearchState -> IO MoveScore
searchZero positions depth endTime rootBest searchState = do
    let position = head positions
    let positionsWithBestFirst = bestMoveFirst position rootBest
    let thisM = snd (head positionsWithBestFirst)
    let best = mkMs (-100000, [thisM])
    highestRatedMoveZero positionsWithBestFirst positions (-100000) 100000 depth endTime best searchState
    where 
        highestRatedMoveZero :: [(Position,Move)] -> [Position] -> Int -> Int -> Int -> Int -> MoveScore -> SearchState -> IO MoveScore
        highestRatedMoveZero [] _ _ _ _ _ best _ = return best
        highestRatedMoveZero (thisP:ps) positions low high depth endTime best searchState = do
            t <- timeMillis
            if t > endTime
                then return best
                else do
                        searchResult <- uncurry search thisP depth (-high) (-low) endTime best searchState
                        let ms = if canLeadToDrawByRepetition (fst thisP) positions
                            then mkMs (1, msPath best)
                            else searchResult
                        let negatedScore = -(msScore ms)
                        if negatedScore > low
                            then do
                                let thisM = snd thisP
                                let best' = MoveScore { msScore = negatedScore, msBound = Exact, msPath = thisM : tail (msPath searchResult) }
                                highestRatedMoveZero ps positions negatedScore high depth endTime best' searchState
                            else highestRatedMoveZero ps positions low high depth endTime best searchState

search :: Position -> Move -> Int -> Int -> Int -> Int -> MoveScore -> SearchState -> IO MoveScore
search position moveZero 0 low high endTime best searchState = do
    q <- quiesce position low high searchState
    return (mkMs (q,[]))
search position moveZero depth low high endTime best searchState = do
    let hpos = zobrist position
    hentry <- H.lookup (hashTable searchState) (calcHashIndex hpos)
    case hashBound depth hpos hentry of
        Just hb -> do
            case hb of
                Exact -> do
                    incNodes 1000000000 searchState
                    let thisM = move (fromJust hentry)
                    return (mkMs (score (fromJust hentry), msPath best ++ [thisM]))
                Lower ->
                    go position moveZero (move (fromJust hentry)) depth (score (fromJust hentry)) high endTime best searchState hpos
                Upper ->
                    go position moveZero (move (fromJust hentry)) depth low (score (fromJust hentry)) endTime best searchState hpos
        Nothing ->
            go position moveZero 0 depth low high endTime best searchState hpos
    where
        go :: Position -> Move -> Move -> Int -> Int -> Int -> Int -> MoveScore -> SearchState -> Int -> IO MoveScore
        go position moveZero _ 0 low high endTime best searchState _ = do
            q <- quiesce position low high searchState
            return (mkMs (q,[]))
        go position moveZero hashMove depth low high endTime best searchState hpos = do
            incNodes 1 searchState
            if halfMoves position == 50
                then return (mkMs (0, msPath best))
                else do
                    t <- timeMillis
                    if t > endTime then return best else do
                        let notInCheckPositions = filter (\(p,m) -> not (isCheck p (mover position))) (newPositions position hashMove)
                        if null notInCheckPositions
                            then return (mkMs (if isCheck position (mover position) then (-9000)-depth else 0, msPath best))
                            else do
                                let thisM = snd (head notInCheckPositions)
                                let best' = MoveScore { msScore=low, msBound=Upper, msPath = msPath best }
                                hrm <- highestRatedMove notInCheckPositions moveZero low high depth endTime best' searchState
                                updateHashTable hpos HashEntry { score=msScore hrm, move=thisM, height=depth, bound=msBound hrm, lock=hpos } searchState
                                return hrm

        highestRatedMove :: [(Position,Move)] -> Move -> Int -> Int -> Int -> Int -> MoveScore -> SearchState -> IO MoveScore
        highestRatedMove [] _ _ _ _ _ best _ = return best
        highestRatedMove notInCheckPositions moveZero low high depth endTime best c = do
            let thisP = head notInCheckPositions
            ms <- search (fst thisP) moveZero (depth-1) (-high) (-low) endTime best c
            let negatedScore = -(msScore ms)
            if negatedScore >= high
                then return ms { msScore=negatedScore, msBound=Lower }
                else do
                    if negatedScore > low
                        then do
                            let thisM = snd thisP
                            let best' = ms { msScore=negatedScore, msBound=Exact, msPath = thisM : msPath best }
                            highestRatedMove (tail notInCheckPositions) moveZero negatedScore high depth endTime best' c
                        else highestRatedMove (tail notInCheckPositions) moveZero low high depth endTime best c

