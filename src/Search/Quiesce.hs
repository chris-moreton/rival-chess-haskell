{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExistentialQuantification, ExplicitNamespaces, FlexibleContexts, FlexibleInstances, KindSignatures, LiberalTypeSynonyms, MultiParamTypeClasses, PostfixOperators, RankNTypes, ScopedTypeVariables, UnicodeSyntax, UnliftedFFITypes #-}

module Search.Quiesce where

import Types
    ( Bound(..),
      HashEntry(..),
      MoveScore(..),
      Position(halfMoves, mover, whiteKingBitboard, blackKingBitboard) )
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
import Util.Bitboards ( exactlyOneBitSet )

quiesce :: Position -> Int -> Int -> Int -> SearchState -> IO Int
quiesce position _ _ 10 searchState = do
    incNodes searchState
    return (evaluate position)
quiesce !position !low !high !ply !searchState = do
    incNodes searchState
    result <- highestQuiesceMove position ms newLow high ply searchState 0
    if snd result == 0 && inCheck
        then return (ply-10000)
        else return (fst result)
    where
        inCheck = isCheck position (mover position)
        newLow = max (evaluate position) low
        ms = quiesceMoves position inCheck
        
        {-# INLINE highestQuiesceMove #-}
        highestQuiesceMove :: Position -> [Move] -> Int -> Int -> Int -> SearchState -> Int -> IO (Int, Int)
        highestQuiesceMove _ [] low _ _ _ validMoves = return (low, validMoves)
        highestQuiesceMove position ms !low !high !depth !searchState validMoves = do
            let thisM = head ms
            let thisP = makeMove position thisM
            if kingCaptured thisP
                then do
                    highestQuiesceMove position (tail ms) low high depth searchState validMoves
                else do
                    score <- quiesce thisP (-high) (-low) (depth+1) searchState
                    let negatedScore = -score
                    if negatedScore >= high
                        then return (negatedScore, validMoves + 1)
                        else highestQuiesceMove position (tail ms) (if negatedScore > low then negatedScore else low) high depth searchState (validMoves + 1)

        {-# INLINE quiesceMoves #-}
        quiesceMoves :: Position -> Bool -> MoveList
        quiesceMoves position inCheck = if inCheck then m else filter (isCapture position) m
            where m = moves position

        kingCaptured :: Position -> Bool
        kingCaptured position = exactlyOneBitSet (whiteKingBitboard position .|. blackKingBitboard position)