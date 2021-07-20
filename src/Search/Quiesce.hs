{-# LANGUAGE BangPatterns #-}
module Search.Quiesce where

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
import Util.Zobrist ( zobrist )
import Search.Evaluate ( evaluate, isCapture, scoreMove )
import Search.SearchHelper ( quiescePositions )

quiesce :: Position -> Int -> Int -> Int -> SearchState -> IO Int
quiesce position _ _ 10 searchState = do
    incNodes 1 searchState
    return (evaluate position)
quiesce !position !low !high !ply !searchState = do
    incNodes 1 searchState
    let eval = evaluate position
    let newLow = max eval low
    let qp = quiescePositions position
    if not (null qp)
        then do
            let notInCheckPositions = filter (\(p,m) -> not (isCheck p (mover position))) qp
            if null notInCheckPositions
                then return (if isCheck position (mover position) then ply-10000 else 0)
                else highestQuiesceMove notInCheckPositions newLow high ply newLow searchState
        else return newLow
    where
        highestQuiesceMove :: [(Position,Move)] -> Int -> Int -> Int -> Int -> SearchState -> IO Int
        highestQuiesceMove [] _ _ _ best _ = return best
        highestQuiesceMove !notInCheckPositions !low !high !depth !best !searchState = do
            let thisP = head notInCheckPositions
            score <- quiesce (fst thisP) (-high) (-low) (depth+1) searchState
            let negatedScore = -score
            if negatedScore >= high
                then return negatedScore
                else do
                    if negatedScore > low
                        then highestQuiesceMove (tail notInCheckPositions) negatedScore high depth negatedScore searchState
                        else highestQuiesceMove (tail notInCheckPositions) low high depth best searchState