{-# LANGUAGE BinaryLiterals,NegativeLiterals,StrictData,BangPatterns #-}

{-# OPTIONS_GHC -Wno-overflowed-literals #-}

module Search.MakeMove where

import Types
import Alias
import Util.Fen
import Util.Utils
import Data.Bits
import Util.Bitboards
import Search.MoveConstants
import Search.MakeSimpleMove
import Search.MoveUtils
import Search.MakeComplexMove

isPotentialFirstKingMove :: Position -> Square -> Bool
{-# INLINE isPotentialFirstKingMove #-}
isPotentialFirstKingMove position from
    | from /= e1Bit && from /= e8Bit = False
    | mover position == White = testBit (whiteKingBitboard position) from
    | otherwise = testBit (blackKingBitboard position) from

isPotentialComplexPawnMove :: Position -> Square -> Square -> Bool
{-# INLINE isPotentialComplexPawnMove #-}
isPotentialComplexPawnMove position from to =
    ((mover position == White && testBit (whitePawnBitboard position) from) || (mover position == Black && testBit (blackPawnBitboard position) from)) && (abs (from - to) /= 8 || to >= 56 || to <= 7)

isSimpleCapture :: Position -> Square -> Bool
{-# INLINE isSimpleCapture #-}
isSimpleCapture position to = testBit (allPiecesBitboard position) to

isSimpleMove :: Position -> Move -> Square -> Square -> Bool
{-# INLINE isSimpleMove #-}
isSimpleMove position move from to = not (isSimpleCapture position to) && not (isPotentialComplexPawnMove position from to) && not (isPotentialFirstKingMove position from)

makeMove :: Position -> Move -> Position
{-# INLINE makeMove #-}
makeMove !position !move = 
    if isSimpleMove position move from to
        then makeSimpleMove position move from
        else makeMoveMain position move
    where from = fromSquarePart move
          to = toSquarePart move





