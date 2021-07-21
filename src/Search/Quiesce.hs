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
    if not (null qp)
        then do
            if null notInCheckPositions
                then return (if isCheck position (mover position) then ply-10000 else 0)
                else highestQuiesceMove notInCheckPositions newLow high ply searchState
        else return newLow
    where
        eval = evaluate position
        newLow = max eval low
        qp = quiescePositions position
        currentMover = mover position
        notInCheckPositions = filter (\(p,m) -> not (isCheck p currentMover)) qp
        
        highestQuiesceMove :: [(Position,Move)] -> Int -> Int -> Int -> SearchState -> IO Int
        highestQuiesceMove [] low _ _ _ = return low
        highestQuiesceMove !notInCheckPositions !low !high !depth !searchState = do
            let thisP = head notInCheckPositions
            score <- quiesce (fst thisP) (-high) (-low) (depth+1) searchState
            let negatedScore = -score
            if negatedScore >= high
                then return negatedScore
                else highestQuiesceMove (tail notInCheckPositions) (if negatedScore > low then negatedScore else low) high depth searchState
