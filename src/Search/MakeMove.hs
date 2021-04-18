module Search.MakeMove where

import Types
import Util.Fen
import Util.Utils

-- This is the section I am currently working on
-- It doesn't do anything useful yet

makeMove :: Position -> Move -> Position
makeMove position move = do
  let fromSquare = fromSquarePart move
  let toSquare = toSquarePart move
  makeSimpleMove position move

makeSimpleMove :: Position -> Move -> Position
makeSimpleMove position move = do
  let m = mover position
  Position {
       positionBitboards = positionBitboards position
     , mover = if m == White then Black else White
     , enPassantSquare = -1
     , positionCastlePrivs = positionCastlePrivs position
     , halfMoves = halfMoves position
     , moveNumber = (+) (moveNumber position) (if m == Black then 1 else 0)
   }
  
  