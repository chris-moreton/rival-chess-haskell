module Search.Search where

import Types ( Position (Position, whitePiecesBitboard, blackPiecesBitboard, enPassantSquare), mover, halfMoves, bitboardForColour, Piece (Pawn, Bishop, Knight, Rook, Queen), Mover (White,Black) )
import Alias ( Move, Bitboard )
import Search.MoveGenerator (moves,isCheck)
import Util.Utils ( timeMillis, toSquarePart )
import Text.Printf
import Util.Fen ( algebraicMoveFromMove )
import Search.MakeMove ( makeMove )
import Data.Bits ( Bits(popCount), Bits(testBit), Bits(bit) )
import Control.Monad
import System.Exit

------------------------------------------------------
-- example of how the IDE simplified my newbie Haskell
------------------------------------------------------
-- any (\oldPos -> p == oldPos) positions
-- any (p ==) positions
-- p `elem` positions
------------------------------------------------------

canLeadToDrawByRepetition :: Position -> [Position] -> Bool
canLeadToDrawByRepetition p ps
    | p `elem` ps = True
    | or ([makeMove p m `elem` ps | m <- moves p]) = True
    | otherwise = False

startSearch :: [Position] -> Int -> Int -> IO (Move,Int)
startSearch (p:ps) maxDepth endTime = do
    let newPositions = map (\move -> (makeMove p move,move)) (moves p)
    let notInCheckPositions = filter (\(p,m) -> not (isCheck p (mover p))) newPositions
    iterativeDeepening (p:ps) 1 maxDepth endTime (snd (head notInCheckPositions),-100000)

iterativeDeepening :: [Position] -> Int -> Int -> Int -> (Move,Int) -> IO (Move,Int)
iterativeDeepening positions depth maxDepth endTime rootBest = do
    result <- searchZero positions depth endTime rootBest
    t <- timeMillis
    if t > endTime || depth == maxDepth
        then return result
        else iterativeDeepening positions (depth+1) maxDepth endTime result

searchZero :: [Position] -> Int -> Int -> (Move,Int) -> IO (Move,Int)
searchZero positions depth endTime rootBest = do
    let position = head positions
    let newPositions = map (\move -> (makeMove position move,move)) (moves position)
    let notInCheckPositions = filter (\(p,m) -> not (isCheck p (mover position))) newPositions
    highestRatedMoveZero notInCheckPositions positions (-100000) 100000 depth endTime (snd (head notInCheckPositions),-100000) rootBest

highestRatedMoveZero :: [(Position,Move)] -> [Position] -> Int -> Int -> Int -> Int -> (Move,Int) -> (Move,Int) -> IO (Move,Int)
highestRatedMoveZero [] _ _ _ _ _ best _ = return best
highestRatedMoveZero (thisP:ps) positions low high depth endTime best rootBest = do
   t <- timeMillis
   if t > endTime
       then return rootBest
       else do
            searchResult <- uncurry search thisP depth (-high) (-low) endTime rootBest
            let (m,s) = if canLeadToDrawByRepetition (fst thisP) positions
                then (snd thisP,1)
                else searchResult
            let negatedScore = -s
            if negatedScore > low
                then highestRatedMoveZero ps positions negatedScore high depth endTime (snd thisP,negatedScore) rootBest
                else highestRatedMoveZero ps positions low high depth endTime best rootBest

search :: Position -> Move -> Int -> Int -> Int -> Int -> (Move,Int) -> IO (Move,Int)
search position moveZero 0 low high endTime _ = return (moveZero,quiesce position low high)
search position moveZero depth low high endTime rootBest = do
    if halfMoves position == 50
        then return (moveZero, 0)
        else do
            t <- timeMillis
            if t > endTime then return rootBest else do
                let notInCheckPositions = filter (\(p,m) -> not (isCheck p (mover position))) (newPositions position)
                if null notInCheckPositions
                    then return (moveZero, if isCheck position (mover position) then (-9000)-depth else 0)
                    else highestRatedMove notInCheckPositions moveZero low high depth endTime (snd (head notInCheckPositions),low) rootBest

highestRatedMove :: [(Position,Move)] -> Move -> Int -> Int -> Int -> Int -> (Move,Int) -> (Move,Int) -> IO (Move,Int)
highestRatedMove [] _ _ _ _ _ best _ = return best
highestRatedMove notInCheckPositions moveZero low high depth endTime best rootBest = do
    let thisP = head notInCheckPositions
    (m,s) <- search (fst thisP) moveZero (depth-1) (-high) (-low) endTime rootBest
    let negatedScore = -s
    if negatedScore >= high
        then return (m,negatedScore)
        else do
            if negatedScore > low
                then highestRatedMove (tail notInCheckPositions) moveZero negatedScore high depth endTime (m,negatedScore) rootBest
                else highestRatedMove (tail notInCheckPositions) moveZero low high depth endTime best rootBest

newPositions :: Position -> [(Position,Move)]
newPositions position = map (\move -> (makeMove position move,move)) (moves position)

material :: Position -> Mover -> Int
material position m = popCount (bitboardForColour position m Pawn) * 100 +
                      popCount (bitboardForColour position m Bishop) * 350 +
                      popCount (bitboardForColour position m Knight) * 350 +
                      popCount (bitboardForColour position m Rook) * 500 +
                      popCount (bitboardForColour position m Queen) * 900

evaluate :: Position -> Int
evaluate position = do
    let whiteScore = material position White - material position Black
    if mover position == White then whiteScore else -whiteScore

isCapture :: Position -> Move -> Bool
isCapture position move
    | m == White = testBit (blackPiecesBitboard position) t || e == t
    | otherwise = testBit (whitePiecesBitboard position) t || e == t
    where m = mover position
          t = toSquarePart move
          e = enPassantSquare position

quiescePositions :: Position -> [(Position,Move)]
quiescePositions position = do
    let m = moves position
    let captures = filter (isCapture position) m
    map (\m -> (makeMove position m,m)) captures

quiesce :: Position -> Int -> Int -> Int
quiesce position low high = quiesceRecur position low high 0

quiesceRecur :: Position -> Int -> Int -> Int -> Int
quiesceRecur position _ _ 10 = evaluate position
quiesceRecur position low high depth = do
    let eval = evaluate position
    let newLow = max eval low
    let qp = quiescePositions position
    let l = length qp
    if not (null qp)
        then do
            let notInCheckPositions = filter (\(p,m) -> not (isCheck p (mover position))) qp
            if null notInCheckPositions
                then if isCheck position (mover position) then (-9000)-depth else 0
                else highestQuiesceMove notInCheckPositions newLow high depth newLow
        else newLow

highestQuiesceMove :: [(Position,Move)] -> Int -> Int -> Int -> Int -> Int
highestQuiesceMove [] _ _ _ best = best
highestQuiesceMove notInCheckPositions low high depth best = do
    let thisP = head notInCheckPositions
    let negatedScore = -(quiesceRecur (fst thisP) (-high) (-low) (depth+1))
    if negatedScore >= high
        then negatedScore
        else do
            if negatedScore > low
                then highestQuiesceMove (tail notInCheckPositions) negatedScore high depth negatedScore
                else highestQuiesceMove (tail notInCheckPositions) low high depth best
