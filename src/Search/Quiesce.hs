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
import Util.Zobrist ( hashIndex, zobrist )
import Search.Evaluate ( evaluate, isCapture, scoreMove )
import Search.SearchHelper

quiesce :: Position -> Int -> Int -> SearchState -> IO Int
quiesce position low high = quiesceRecur position low high 0
    where
        quiesceRecur :: Position -> Int -> Int -> Int -> SearchState -> IO Int
        quiesceRecur position _ _ 10 searchState = do
            incNodes 1 searchState
            return (evaluate position)
        quiesceRecur position low high depth searchState = do
            incNodes 1 searchState
            let eval = evaluate position
            let newLow = max eval low
            let qp = quiescePositions position
            let l = length qp
            if not (null qp)
                then do
                    let notInCheckPositions = filter (\(p,m) -> not (isCheck p (mover position))) qp
                    if null notInCheckPositions
                        then return (if isCheck position (mover position) then (-9000)-depth else 0)
                        else highestQuiesceMove notInCheckPositions newLow high depth newLow searchState
                else return newLow

        highestQuiesceMove :: [(Position,Move)] -> Int -> Int -> Int -> Int -> SearchState -> IO Int
        highestQuiesceMove [] _ _ _ best _ = return best
        highestQuiesceMove notInCheckPositions low high depth best searchState = do
            let thisP = head notInCheckPositions
            score <- quiesceRecur (fst thisP) (-high) (-low) (depth+1) searchState
            let negatedScore = -score
            if negatedScore >= high
                then return negatedScore
                else do
                    if negatedScore > low
                        then highestQuiesceMove (tail notInCheckPositions) negatedScore high depth negatedScore searchState
                        else highestQuiesceMove (tail notInCheckPositions) low high depth best searchState