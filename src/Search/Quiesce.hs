{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExistentialQuantification, ExplicitNamespaces, FlexibleContexts, FlexibleInstances, KindSignatures, LiberalTypeSynonyms, MultiParamTypeClasses, PostfixOperators, RankNTypes, ScopedTypeVariables, UnicodeSyntax, UnliftedFFITypes #-}

module Search.Quiesce where

import Types
    ( Bound(..),
      HashEntry(..),
      MoveScore(..),
      Position(halfMoves, mover),
      HashEntry )
import Alias ( Move, Bitboard, MoveList, Path )
import Search.MoveGenerator (moves,isCheck,captureMoves)
import Util.Utils ( timeMillis, toSquarePart )
import Text.Printf ()
import Util.Fen ( algebraicMoveFromMove )
import Search.MakeMove ( makeMove )
import Data.Bits ( Bits(popCount), Bits(testBit), Bits(bit), (.|.), (.&.), clearBit, shiftL )
import Control.Monad ()
import System.Exit ()
import State.State
import Data.Sort ( sortBy )
import Data.Maybe ( isJust, fromJust )
import State.State ( incNodes, updateHashTable, SearchState(..), calcHashIndex, setPv )
import qualified Data.HashTable.IO as H
import Util.Zobrist ( zobrist )
import Search.SearchHelper ( sortMoves, mkMs )
import Evaluate.Evaluate ( evaluate, isCapture, scoreMove )

goQuiesce :: Position -> Int -> Int -> Int -> SearchState -> IO MoveScore
goQuiesce !position !low !high !ply !searchState = quiesce position low high ply searchState 2

quiesce :: Position -> Int -> Int -> Int -> SearchState -> Int -> IO MoveScore
quiesce position _ _ 100 searchState _ = do
    incNodes searchState
    return $ MoveScore { msScore=evaluate position, msPath=[], msBound=Exact }
quiesce !position !low !high !ply !searchState !maxChecks = do
    incNodes searchState
    if null notInCheckPositions
        then return MoveScore { msScore=if inCheck then ply-10000 else startLow, msPath=[], msBound=Exact }
        else do
            let thisM = snd (head notInCheckPositions)
            let best = MoveScore { msScore=startLow, msBound=Upper, msPath = [thisM] }
            highestQuiesceMove notInCheckPositions startLow high best
    where
        eval = evaluate position
        inCheck = maxChecks > 0 && isCheck position (mover position)
        startLow = if inCheck then low else max eval low
        notInCheckPositions = quiescePositions position inCheck
        newMaxChecks = if inCheck then maxChecks-1 else maxChecks

        highestQuiesceMove :: [(Position,Move)] -> Int -> Int -> MoveScore -> IO MoveScore
        highestQuiesceMove [] _ _ best = return best
        highestQuiesceMove !notInCheckPositions !low !high best = do
            let (thisP,thisM) = head notInCheckPositions
            ms <- quiesce thisP (-high) (-low) (ply+1) searchState newMaxChecks
            let negatedScore = -(msScore ms)
            if negatedScore >= high
                then return ms { msScore=negatedScore, msBound=Lower, msPath = thisM : msPath ms }
                else do
                    if negatedScore > low
                        then do
                             let best' = ms { msScore=negatedScore, msBound=Exact, msPath = thisM : msPath ms }
                             highestQuiesceMove (tail notInCheckPositions) negatedScore high best'
                        else highestQuiesceMove (tail notInCheckPositions) low high best

{-# INLINE quiescePositions #-}
quiescePositions :: Position -> Bool -> [(Position,Move)]
quiescePositions position inCheck =
    if null ps && inCheck
        then filter (\(p,m) -> not (isCheck p $ mover position)) $ map (\m -> (makeMove position m, m)) (moves position)
        else ps
    where
        ps = filter (\(p,m) -> not (isCheck p $ mover position)) $ map (\m -> (makeMove position m, m)) (sortMoves position 0 $ captureMoves position)

            