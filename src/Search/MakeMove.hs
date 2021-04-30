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
import Search.MakeComplextMove

makeMove :: Position -> Move -> Position
makeMove !position !move = 
    if not (testBit (allPiecesBitboard position) to) 
        then makeSimpleMove position move from
        else makeMoveMain position move
    where from = fromSquarePart move
          to = toSquarePart move




