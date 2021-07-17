{-# LANGUAGE BinaryLiterals #-}

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
import Util.Zobrist ( hashIndex, zobrist )
import Search.Evaluate ( evaluate, isCapture, scoreMove )

canLeadToDrawByRepetition :: Position -> [Position] -> Bool
canLeadToDrawByRepetition p ps
    | p `elem` ps = True
    | or ([makeMove p m `elem` ps | m <- moves p]) = True
    | otherwise = False

mkMs :: (Int,Path) -> MoveScore
mkMs (score, path) = MoveScore { msScore=score, msBound=Exact, msPath=path }

sortMoves :: Position -> Move -> MoveList -> MoveList
sortMoves position hashMove moves = do
    let scoredMoves = map (\m -> m + scoreMove position hashMove m `shiftL` 32) moves
    map (0b0000000000000000000000000000000011111111111111111111111111111111 .&.) (sortBy (flip compare) scoredMoves)

bestMoveFirst :: Position -> Move -> [(Position,Move)]
bestMoveFirst position bestMove = do
    let movesFromPosition = moves position
    let sortedMoves = sortMoves position bestMove movesFromPosition
    let newPositions = map (\move -> (makeMove position move,move)) sortedMoves
    let notInCheckPositions = filter (\(p,m) -> not (isCheck p (mover position))) newPositions
    notInCheckPositions

hashBound :: Int -> Int -> Maybe HashEntry -> Maybe Bound
hashBound depth lockVal he =
     case he of
         Just x -> if height x >= depth && lock x == lockVal then return (bound x) else Nothing
         _      -> Nothing

newPositions :: Position -> Move -> [(Position,Move)]
newPositions position hashMove = map (\move -> (makeMove position move,move)) (sortMoves position hashMove (moves position))

quiescePositions :: Position -> [(Position,Move)]
quiescePositions position = do
    let m = moves position
    map (\m -> (makeMove position m,m)) 
        (if isCheck position (mover position) then m else filter (isCapture position) m)


