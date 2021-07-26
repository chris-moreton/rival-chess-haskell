{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE ExistentialQuantification, ExplicitNamespaces, FlexibleContexts, FlexibleInstances, KindSignatures, LiberalTypeSynonyms, MultiParamTypeClasses, PostfixOperators, RankNTypes, ScopedTypeVariables, UnicodeSyntax, UnliftedFFITypes #-}

module Search.SearchHelper where

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

{-# INLINE canLeadToDrawByRepetition #-}
canLeadToDrawByRepetition :: Position -> [Position] -> Bool
canLeadToDrawByRepetition p ps
    | p `elem` ps = True
    | or ([makeMove p m `elem` ps | m <- moves p]) = True
    | otherwise = False

{-# INLINE mkMs #-}
mkMs :: (Int,Path) -> MoveScore
mkMs (score, path) = MoveScore { msScore=score, msBound=Exact, msPath=path }

{-# INLINE sortMoves #-}
sortMoves :: Position -> Move -> MoveList -> MoveList
sortMoves position hashMove moves = do
    let scoredMoves = map (\m -> m + scoreMove position hashMove m `shiftL` 32) moves
    map (0b0000000000000000000000000000000011111111111111111111111111111111 .&.) (sortBy (flip compare) scoredMoves)

{-# INLINE bestMoveFirst #-}
bestMoveFirst :: Position -> Move -> [(Position,Move)]
bestMoveFirst position bestMove = do
    let movesFromPosition = moves position
    let sortedMoves = sortMoves position bestMove movesFromPosition
    let newPositions = map (\move -> (makeMove position move,move)) sortedMoves
    let notInCheckPositions = filter (\(p,m) -> not (isCheck p (mover position))) newPositions
    notInCheckPositions

{-# INLINE hashBound #-}
hashBound :: Int -> Int -> Maybe HashEntry -> Maybe Bound
hashBound depth lockVal he =
     case he of
         Just x -> if height x >= depth && lock x == lockVal then return (bound x) else Nothing
         _      -> Nothing

{-# INLINE newPositions #-}
newPositions :: Position -> Move -> [(Position,Move)]
newPositions position hashMove = map (\move -> (makeMove position move,move)) (sortMoves position hashMove (moves position))



