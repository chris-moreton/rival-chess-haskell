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
quiesce position _ _ 100 searchState = do
    incNodes searchState
    return (evaluate position)
quiesce !position !low !high !ply !searchState = do
    incNodes searchState
    highestQuiesceMove position ms newLow high ply searchState
    where
        newLow  = max (evaluate position) low
        m       = moves position
        ms      = filter (isCapture position) m
        
        {-# INLINE highestQuiesceMove #-}
        highestQuiesceMove :: Position -> [Move] -> Int -> Int -> Int -> SearchState -> IO Int
        highestQuiesceMove _ [] low _ _ _ = return low
        highestQuiesceMove position ms !low !high !ply !searchState = do
            let thisM = head ms
            let thisP = makeMove position thisM
            if kingCaptured thisP
                then do
                    return 10001
                else do
                    score <- quiesce thisP (-high) (-low) (ply+1) searchState
                    let negatedScore = -score
                    if negatedScore >= high
                        then return negatedScore
                        else highestQuiesceMove position (tail ms) (if negatedScore > low then negatedScore else low) high ply searchState

        kingCaptured :: Position -> Bool
        kingCaptured position = exactlyOneBitSet (whiteKingBitboard position .|. blackKingBitboard position)