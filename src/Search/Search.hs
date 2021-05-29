{-# LANGUAGE BinaryLiterals #-}

module Search.Search where

import Types ( Position (..), mover, halfMoves, bitboardForColour, Piece (..), Mover (White,Black) )
import Alias ( Move, Bitboard, MoveList )
import Search.MoveGenerator (moves,isCheck)
import Util.Utils ( timeMillis, toSquarePart )
import Text.Printf ()
import Util.Fen ( algebraicMoveFromMove )
import Search.MakeMove ( makeMove )
import Data.Bits ( Bits(popCount), Bits(testBit), Bits(bit), (.|.), (.&.), clearBit, shiftL )
import Control.Monad ()
import System.Exit ()
import Data.Sort ( sortBy )
import Data.IORef

data Counter = Counter { x :: IORef Int }

makeCounter :: Int -> IO Counter        
makeCounter i = do iref <- newIORef i   
                   return (Counter iref)

incCounter :: Int -> Counter -> IO ()            
incCounter i (Counter c) = do modifyIORef c (+ i)

decCounter :: Int -> Counter -> IO ()            
decCounter i (Counter c) = do modifyIORef c ((-) i)

showCounter :: Counter -> IO ()               
showCounter (Counter c) = do c' <- readIORef c
                             print(c')  

hashPosition :: Position -> Int
hashPosition p =
    (if mover p == White then 1238799 else 12389876) + (2 * enPassantSquare p) + (3 * whitePawnBitboard  p) +
    (4 * blackPawnBitboard p) + (5 * whiteKnightBitboard  p) + (6 * blackKnightBitboard p) +
    (7 * whiteBishopBitboard p) + (8 * blackBishopBitboard p) +
    (11 * whiteRookBitboard p) + (12 * blackRookBitboard p) +
    (13 * whiteQueenBitboard p) + (14 * blackQueenBitboard p) +
    (15 * whiteKingBitboard p) + (16 * blackKingBitboard p)

canLeadToDrawByRepetition :: Position -> [Position] -> Bool
canLeadToDrawByRepetition p ps
    | p `elem` ps = True
    | or ([makeMove p m `elem` ps | m <- moves p]) = True
    | otherwise = False

startSearch :: [Position] -> Int -> Int -> Counter -> IO (Move,Int)
startSearch (position:positions) maxDepth endTime c = do
    let theseMoves = sortMoves position (moves position)
    let newPositions = map (\move -> (makeMove position move,move)) theseMoves
    let notInCheckPositions = filter (\(p,m) -> not (isCheck p (mover position))) newPositions
    iterativeDeepening (position:positions) 1 maxDepth endTime (snd (head notInCheckPositions),-100000) c

iterativeDeepening :: [Position] -> Int -> Int -> Int -> (Move,Int) -> Counter -> IO (Move,Int)
iterativeDeepening positions depth maxDepth endTime rootBest c = do
    result <- searchZero positions depth endTime rootBest c
    t <- timeMillis
    if t > endTime || depth == maxDepth
        then return result
        else iterativeDeepening positions (depth+1) maxDepth endTime result c

captureScore :: Position -> Move -> Int
captureScore position move
    | isCapture position move = pieceValue (capturePiece position move)
    | otherwise = 0

centreScore :: Position -> Move -> Int
centreScore position move
    | 0b0000000000000000001111000011110000111100001111000000000000000000 .&. toSquareMask /= 0 = 10
    | otherwise = 0
    where toSquareMask = bit (toSquarePart move) :: Bitboard

scoreMove :: Position -> Move -> Int
scoreMove position move = captureScore position move + centreScore position move

sortMoves :: Position -> MoveList -> MoveList
sortMoves position moves = do
    let scoredMoves = map (\m -> m + (scoreMove position m `shiftL` 32)) moves
    map (0b0000000000000000000000000000000011111111111111111111111111111111 .&.) (sortBy (flip compare) scoredMoves)

bestMoveFirst :: Position -> (Move,Int) -> [(Position,Move)]
bestMoveFirst position best = do
    let movesWithoutBest = sortMoves position (filter (\m -> m /= snd best) (moves position))
    let newPositionsWithoutBest = map (\move -> (makeMove position move,move)) movesWithoutBest
    let bestPosition = (makeMove position (fst best),fst best)
    let notInCheckPositions = filter (\(p,m) -> not (isCheck p (mover position))) newPositionsWithoutBest
    bestPosition : notInCheckPositions

searchZero :: [Position] -> Int -> Int -> (Move,Int) -> Counter -> IO (Move,Int)
searchZero positions depth endTime rootBest c = do
    let position = head positions
    let positionsWithBestFirst = bestMoveFirst position rootBest
    highestRatedMoveZero (bestMoveFirst position rootBest) positions (-100000) 100000 depth endTime (snd (head positionsWithBestFirst),-100000) rootBest c

highestRatedMoveZero :: [(Position,Move)] -> [Position] -> Int -> Int -> Int -> Int -> (Move,Int) -> (Move,Int) -> Counter -> IO (Move,Int)
highestRatedMoveZero [] _ _ _ _ _ best _ _ = return best
highestRatedMoveZero (thisP:ps) positions low high depth endTime best rootBest c = do
   t <- timeMillis
   if t > endTime
       then return best
       else do
            searchResult <- uncurry search thisP depth (-high) (-low) endTime rootBest c
            let (m,s) = if canLeadToDrawByRepetition (fst thisP) positions
                then (snd thisP,1)
                else searchResult
            let negatedScore = -s
            if negatedScore > low
                then highestRatedMoveZero ps positions negatedScore high depth endTime (snd thisP,negatedScore) rootBest c
                else highestRatedMoveZero ps positions low high depth endTime best rootBest c

search :: Position -> Move -> Int -> Int -> Int -> Int -> (Move,Int) -> Counter -> IO (Move,Int)
search position moveZero 0 low high endTime _ c = do
    incCounter 1 c
    return (moveZero,quiesce position low high)
search position moveZero depth low high endTime rootBest c = do
    incCounter 1 c
    if halfMoves position == 50
        then return (moveZero, 0)
        else do
            t <- timeMillis
            if t > endTime then return rootBest else do
                let notInCheckPositions = filter (\(p,m) -> not (isCheck p (mover position))) (newPositions position)
                if null notInCheckPositions
                    then return (moveZero, if isCheck position (mover position) then (-9000)-depth else 0)
                    else highestRatedMove notInCheckPositions moveZero low high depth endTime (snd (head notInCheckPositions),low) rootBest c

highestRatedMove :: [(Position,Move)] -> Move -> Int -> Int -> Int -> Int -> (Move,Int) -> (Move,Int) -> Counter -> IO (Move,Int)
highestRatedMove [] _ _ _ _ _ best _ _ = return best
highestRatedMove notInCheckPositions moveZero low high depth endTime best rootBest c = do
    let thisP = head notInCheckPositions
    (m,s) <- search (fst thisP) moveZero (depth-1) (-high) (-low) endTime rootBest c
    let negatedScore = -s
    if negatedScore >= high
        then return (m,negatedScore)
        else do
            if negatedScore > low
                then highestRatedMove (tail notInCheckPositions) moveZero negatedScore high depth endTime (m,negatedScore) rootBest c
                else highestRatedMove (tail notInCheckPositions) moveZero low high depth endTime best rootBest c

newPositions :: Position -> [(Position,Move)]
newPositions position = map (\move -> (makeMove position move,move)) (moves position)

pieceValue :: Piece -> Int
pieceValue Pawn = 100
pieceValue Knight = 350
pieceValue Bishop = 350
pieceValue Rook = 500
pieceValue Queen = 900
pieceValue King = 3000

material :: Position -> Mover -> Int
material position m = popCount (bitboardForColour position m Pawn) * pieceValue Pawn +
                      popCount (bitboardForColour position m Bishop) * pieceValue Bishop +
                      popCount (bitboardForColour position m Knight) * pieceValue Knight +
                      popCount (bitboardForColour position m Rook) * pieceValue Rook +
                      popCount (bitboardForColour position m Queen) * pieceValue Queen

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

capturePiece :: Position -> Move -> Piece
capturePiece position move
    | e == t = Pawn
    | testBit (whitePawnBitboard position) t = Pawn
    | testBit (blackPawnBitboard position) t = Pawn
    | testBit (whiteKnightBitboard position) t = Knight
    | testBit (blackKnightBitboard position) t = Knight
    | testBit (whiteBishopBitboard position) t = Bishop
    | testBit (blackBishopBitboard position) t = Bishop
    | testBit (whiteRookBitboard position) t = Rook
    | testBit (blackRookBitboard position) t = Rook
    | testBit (whiteQueenBitboard position) t = Queen
    | testBit (blackQueenBitboard position) t = Queen
    where t = toSquarePart move
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
