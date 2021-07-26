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
import Evaluate.Evaluate ( evaluate, isCapture, scoreMove )
import Search.SearchHelper ( quiescePositions )

quiesce :: Position -> Int -> Int -> Int -> SearchState -> IO Int
quiesce position _ _ 10 searchState = do
    incNodes searchState
    return (evaluate position)
quiesce !position !low !high !ply !searchState = do
    incNodes searchState
    if null notInCheckPositions
        then return (if isCheck position (mover position) then ply-10000 else 0)
        else highestQuiesceMove notInCheckPositions newLow high ply searchState
    where
        newLow = max (evaluate position) low
        notInCheckPositions = filter (\p -> not (isCheck p $ mover position)) $ quiescePositions position
        
        highestQuiesceMove :: [Position] -> Int -> Int -> Int -> SearchState -> IO Int
        highestQuiesceMove [] low _ _ _ = return low
        highestQuiesceMove !notInCheckPositions !low !high !depth !searchState = do
            score <- quiesce (head notInCheckPositions) (-high) (-low) (depth+1) searchState
            let negatedScore = -score
            if negatedScore >= high
                then return negatedScore
                else highestQuiesceMove (tail notInCheckPositions) (if negatedScore > low then negatedScore else low) high depth searchState
