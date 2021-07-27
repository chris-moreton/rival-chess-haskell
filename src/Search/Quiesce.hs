{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExistentialQuantification, ExplicitNamespaces, FlexibleContexts, FlexibleInstances, KindSignatures, LiberalTypeSynonyms, MultiParamTypeClasses, PostfixOperators, RankNTypes, ScopedTypeVariables, UnicodeSyntax, UnliftedFFITypes #-}

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
import Search.SearchHelper ( sortMoves )
import Evaluate.Evaluate ( evaluate, isCapture, scoreMove )

goQuiesce :: Position -> Int -> Int -> Int -> SearchState -> IO Int
goQuiesce !position !low !high !ply !searchState = quiesce position low high ply searchState 2

quiesce :: Position -> Int -> Int -> Int -> SearchState -> Int -> IO Int
quiesce position _ _ 100 searchState _ = do
    incNodes searchState
    return (evaluate position)
quiesce !position !low !high !ply !searchState !maxChecks = do
    incNodes searchState
    if null notInCheckPositions
        then return (if inCheck then ply-10000 else newLow)
        else highestQuiesceMove notInCheckPositions newLow high
    where
        eval = evaluate position
        inCheck = maxChecks > 0 && isCheck position (mover position)
        newLow = if inCheck then low else max eval low
        qp = quiescePositions position inCheck
        notInCheckPositions = filter (\p -> not (isCheck p $ mover position)) qp
        newMaxChecks = if inCheck then maxChecks - 1 else maxChecks
        
        highestQuiesceMove :: [Position] -> Int -> Int -> IO Int
        highestQuiesceMove [] low _ = return low
        highestQuiesceMove !notInCheckPositions !low !high = do
            score <- quiesce (head notInCheckPositions) (-high) (-low) (ply+1) searchState newMaxChecks
            let negatedScore = -score
            if negatedScore >= high
                then return negatedScore
                else highestQuiesceMove (tail notInCheckPositions) (if negatedScore > low then negatedScore else low) high

{-# INLINE quiescePositions #-}
quiescePositions :: Position -> Bool -> [Position]
quiescePositions position inCheck = do
    let m = moves position
    map (makeMove position) $ if inCheck then m else filter (isCapture position) m