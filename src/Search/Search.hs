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
import State.State ( incNodes, updateHashTable, SearchState(..), calcHashIndex, setPv, showPv, pathString )
import qualified Data.HashTable.IO as H
import Util.Zobrist ( hashIndex, zobrist )
import Search.Evaluate ( evaluate, isCapture, scoreMove )
import Search.SearchHelper
    ( newPositions,
      hashBound,
      canLeadToDrawByRepetition,
      mkMs,
      bestMoveFirst,
      sortMoves )
import Search.Quiesce ( quiesce )

startSearch :: [Position] -> Int -> Int -> SearchState -> IO MoveScore
startSearch (position:positions) maxDepth endTime searchState = do
    let theseMoves = sortMoves position 0 (moves position)
    let newPositions = map (\move -> (makeMove position move,move)) theseMoves
    let notInCheckPositions = filter (\(p,m) -> not (isCheck p (mover position))) newPositions
    result <- iterativeDeepening (position:positions) 1 maxDepth endTime (snd (head notInCheckPositions)) searchState
    setPv (msPath result) searchState
    return result
    where
        iterativeDeepening :: [Position] -> Int -> Int -> Int -> Move -> SearchState -> IO MoveScore
        iterativeDeepening positions depth maxDepth endTime rootBest searchState = do
            result <- searchZero positions depth endTime rootBest searchState
            t <- timeMillis
            if t > endTime || depth == maxDepth
                then return result
                else iterativeDeepening positions (depth+1) maxDepth endTime (head $ msPath result) searchState

searchZero :: [Position] -> Int -> Int -> Move -> SearchState -> IO MoveScore
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
                        searchResult <- uncurry search thisP (depth-1) (-high) (-low) endTime searchState
                        let ms = if canLeadToDrawByRepetition (fst thisP) positions
                            then mkMs (1, msPath best)
                            else searchResult
                        let negatedScore = -(msScore ms)
                        if negatedScore > low
                            then do
                                let thisM = snd thisP
                                let path' = thisM : msPath searchResult
                                let best' = MoveScore { msScore = negatedScore, msBound = Exact, msPath = path' }
                                let pvText = pathString path' (fst thisP) ""
                                let output = "info score cp " ++ show negatedScore ++ " pv" ++ pvText
                                putStrLn output
                                highestRatedMoveZero ps positions negatedScore high depth endTime best' searchState
                            else highestRatedMoveZero ps positions low high depth endTime best searchState

search :: Position -> Move -> Int -> Int -> Int -> Int -> SearchState -> IO MoveScore
search inPosition inMove 0 low high endTime searchState = do
    q <- quiesce inPosition low high searchState
    return (mkMs (q,[]))
search inPosition inMove depth low high endTime searchState = do
    let hpos = zobrist inPosition
    let hashIndex = calcHashIndex hpos
    hentry <- H.lookup (hashTable searchState) hashIndex
    let hashTablePath = hePath (fromJust hentry)
    case hashBound depth hpos hentry of
        Just hb -> do
            let hashTableMove = head hashTablePath
            case hb of
                Exact -> do
                    incNodes 1000000000 searchState
                    return (mkMs (score (fromJust hentry), hashTablePath))
                Lower ->
                    go inPosition inMove hashTableMove depth (score (fromJust hentry)) high endTime searchState hpos
                Upper ->
                    go inPosition inMove hashTableMove depth low (score (fromJust hentry)) endTime searchState hpos
        Nothing ->
            go inPosition inMove 0 depth low high endTime searchState hpos
    where
        go :: Position -> Move -> Move -> Int -> Int -> Int -> Int -> SearchState -> Int -> IO MoveScore
        go inPosition inMove _ 0 low high endTime searchState _ = do
            q <- quiesce inPosition low high searchState
            return (mkMs (q,[]))
        go inPosition inMove hashMove depth low high endTime searchState hpos = do
            incNodes 1 searchState
            if halfMoves inPosition == 50
                then return (mkMs (0, []))
                else do
                    t <- timeMillis
                    if t > endTime
                        then do
                            return MoveScore { msScore = -100000, msBound = Lower, msPath = [] }
                        else do
                            let notInCheckPositions = filter (\(p,m) -> not (isCheck p (mover inPosition))) (newPositions inPosition hashMove)
                            if null notInCheckPositions
                                then return (mkMs (if isCheck inPosition (mover inPosition) then (-9000)-depth else 0, []))
                                else do
                                    let thisM = snd (head notInCheckPositions)
                                    let best' = MoveScore { msScore=low, msBound=Upper, msPath = [thisM] }
                                    hrm <- highestRatedMove notInCheckPositions low high depth endTime best' searchState
                                    updateHashTable hpos HashEntry { score = msScore hrm, hePath = msPath hrm, height = depth, bound = msBound hrm, lock = hpos } searchState
                                    return hrm

        highestRatedMove :: [(Position,Move)] -> Int -> Int -> Int -> Int -> MoveScore -> SearchState -> IO MoveScore
        highestRatedMove [] _ _ _ _ best _ = return best
        highestRatedMove notInCheckPositions low high depth endTime best c = do
            let thisPM = head notInCheckPositions
            let thisM = snd thisPM
            ms <- uncurry search thisPM (depth-1) (-high) (-low) endTime c
            let negatedScore = -(msScore ms)
            if negatedScore >= high
                then return ms { msScore=negatedScore, msBound=Lower, msPath = thisM : msPath ms }
                else do
                    if negatedScore > low
                        then do
                            let best' = ms { msScore=negatedScore, msBound=Exact, msPath = thisM : msPath ms }
                            highestRatedMove (tail notInCheckPositions) negatedScore high depth endTime best' c
                        else highestRatedMove (tail notInCheckPositions) low high depth endTime best c

