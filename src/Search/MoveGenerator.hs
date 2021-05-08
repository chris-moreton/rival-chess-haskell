{-# LANGUAGE StrictData,BangPatterns #-}

{-# OPTIONS_GHC -Wno-overflowed-literals #-}

module Search.MoveGenerator where

import Types
import Alias
import Data.Word
import Data.Bits
import Util.Bitboards
import Util.MagicBitboards
import Util.Utils
import Search.MoveConstants
import Data.Array.IArray
import Control.Parallel
import Control.Monad.Par

bitboardForMover :: Position -> Piece -> Bitboard
{-# INLINE bitboardForMover #-}
bitboardForMover !position = bitboardForColour position (mover position)

bitboardForColour :: Position -> Mover -> Piece -> Bitboard
{-# INLINE bitboardForColour #-}
bitboardForColour !pieceBitboards White King = whiteKingBitboard pieceBitboards
bitboardForColour !pieceBitboards White Queen = whiteQueenBitboard pieceBitboards
bitboardForColour !pieceBitboards White Rook = whiteRookBitboard pieceBitboards
bitboardForColour !pieceBitboards White Knight = whiteKnightBitboard pieceBitboards
bitboardForColour !pieceBitboards White Bishop = whiteBishopBitboard pieceBitboards
bitboardForColour !pieceBitboards White Pawn = whitePawnBitboard pieceBitboards
bitboardForColour !pieceBitboards Black King = blackKingBitboard pieceBitboards
bitboardForColour !pieceBitboards Black Queen = blackQueenBitboard pieceBitboards
bitboardForColour !pieceBitboards Black Rook = blackRookBitboard pieceBitboards
bitboardForColour !pieceBitboards Black Knight = blackKnightBitboard pieceBitboards
bitboardForColour !pieceBitboards Black Bishop = blackBishopBitboard pieceBitboards
bitboardForColour !pieceBitboards Black Pawn = blackPawnBitboard pieceBitboards

bitRefList :: Bitboard -> [Square]
{-# INLINE bitRefList #-}
bitRefList !bitboard = recurBitRefList bitboard (popCount bitboard) []

recurBitRefList :: Bitboard -> Int -> [Square] -> [Square]
{-# INLINE recurBitRefList #-}
recurBitRefList _ 0 !result = result
recurBitRefList !bitboard 1 !result = (countTrailingZeros bitboard) : result
recurBitRefList !bitboard !popcount !result = recurBitRefList (xor bitboard (bit square)) (popcount - 1) (square : result) where square = countTrailingZeros bitboard

allBitsExceptFriendlyPieces :: Position -> Bitboard
{-# INLINE allBitsExceptFriendlyPieces #-}
allBitsExceptFriendlyPieces !position = complement (if mover position == White then whitePiecesBitboard position else blackPiecesBitboard position)

movesFromToSquaresBitboard :: Square -> Bitboard -> MoveList
{-# INLINE movesFromToSquaresBitboard #-}
movesFromToSquaresBitboard !fromSquare !toSquares = recurMovesFromToSquaresBitboard (shiftL fromSquare 16) toSquares []

recurMovesFromToSquaresBitboard :: Square -> Bitboard -> MoveList -> MoveList
{-# INLINE recurMovesFromToSquaresBitboard #-}
recurMovesFromToSquaresBitboard _ 0 !result = result
recurMovesFromToSquaresBitboard !fromSquare !toSquares !result = 
    recurMovesFromToSquaresBitboard fromSquare (xor toSquares (bit square)) ((.|.) fromSquare square : result)
    where square = countTrailingZeros toSquares

generateKnightMoves :: Position -> MoveList
{-# INLINE generateKnightMoves #-}
generateKnightMoves !position = recurKnightMoves position (bitRefList (bitboardForMover position Knight)) []

recurKnightMoves :: Position -> [Square] -> MoveList -> MoveList
{-# INLINE recurKnightMoves #-}
recurKnightMoves _ [] !result = result
recurKnightMoves !position !fromSquares !result =
    recurKnightMoves position (tail fromSquares) (result ++ movesFromToSquaresBitboard fromSquare ((.&.) (knightMovesBitboards fromSquare) (allBitsExceptFriendlyPieces position)))
        where fromSquare = head fromSquares

generateKingMoves :: Position -> MoveList
{-# INLINE generateKingMoves #-}
generateKingMoves !position =
    movesFromToSquaresBitboard kingSquare ((.&.) (kingMovesBitboards kingSquare) (allBitsExceptFriendlyPieces position))
        where kingSquare = countTrailingZeros (bitboardForMover position King)

generateBishopMoves :: Position -> MoveList
{-# INLINE generateBishopMoves #-}
generateBishopMoves position = generateSliderMoves position Bishop

generateRookMoves :: Position -> MoveList
{-# INLINE generateRookMoves #-}
generateRookMoves !position = generateSliderMoves position Rook

generateSliderMoves :: Position -> Piece -> MoveList
{-# INLINE generateSliderMoves #-}
generateSliderMoves !position !piece = recurGenerateSliderMoves fromSquares position magicVars []
    where !bitboards = position
          !magicVars = if piece == Bishop then magicBishopVars else magicRookVars
          !thisMover = mover position
          !bitboard = bitboardForColour bitboards thisMover piece .|. bitboardForColour bitboards thisMover Queen
          !fromSquares = bitRefList bitboard

recurGenerateSliderMoves :: [Square] -> Position -> MagicVars -> MoveList -> MoveList
{-# INLINE recurGenerateSliderMoves #-}
recurGenerateSliderMoves [] _ _ !result = result
recurGenerateSliderMoves !fromSquares !position !magicVars !result =
  recurGenerateSliderMoves (tail fromSquares) position magicVars (result ++ thisResult)
    where !fromSquare = head fromSquares
          !numberMagic = magicNumber magicVars fromSquare
          !shiftMagic = magicNumberShifts magicVars fromSquare
          !maskMagic = occupancyMask magicVars fromSquare
          !occupancy = (.&.) (allPiecesBitboard position) maskMagic
          !rawIndex = fromIntegral (occupancy * numberMagic) :: Word
          !toSquaresMagicIndex = fromIntegral(shiftR rawIndex shiftMagic) :: Int
          !toSquaresBitboard = (.&.) (magic magicVars fromSquare toSquaresMagicIndex) (allBitsExceptFriendlyPieces position)
          !thisResult = recurGenerateSliderMovesWithToSquares (fromSquareMask fromSquare) toSquaresBitboard []

recurGenerateSliderMovesWithToSquares :: Square -> Bitboard -> MoveList -> MoveList
{-# INLINE recurGenerateSliderMovesWithToSquares #-}
recurGenerateSliderMovesWithToSquares !fromSquare 0 !result = result
recurGenerateSliderMovesWithToSquares !fromSquare !toSquares !result =
    recurGenerateSliderMovesWithToSquares fromSquare (xor toSquares (bit square)) ((.|.) fromSquare square : result)
    where !square = countTrailingZeros toSquares

promotionMoves :: Move -> MoveList
{-# INLINE promotionMoves #-}
promotionMoves !move = [(.|.) move promotionQueenMoveMask, (.|.) move promotionRookMoveMask, (.|.) move promotionBishopMoveMask, (.|.) move promotionKnightMoveMask]

generatePawnMovesFromToSquares :: Square -> Bitboard -> MoveList
{-# INLINE generatePawnMovesFromToSquares #-}
generatePawnMovesFromToSquares !fromSquare !toSquares = recurGeneratePawnMovesFromToSquares (fromSquareMask fromSquare) toSquares []

recurGeneratePawnMovesFromToSquares :: Move -> Bitboard -> MoveList -> MoveList
{-# INLINE recurGeneratePawnMovesFromToSquares #-}
recurGeneratePawnMovesFromToSquares _ !0 !result = result
recurGeneratePawnMovesFromToSquares !mask !toSquares !result = recurGeneratePawnMovesFromToSquares mask (xor toSquares (bit thisToSquare)) newResult
  where !thisToSquare = countTrailingZeros toSquares
        !baseMove = (.|.) mask thisToSquare
        !newResult = if thisToSquare >= 56 || thisToSquare <= 7
                        then promotionMoves baseMove ++ result
                        else baseMove : result

generatePawnMoves :: Position -> MoveList
{-# INLINE generatePawnMoves #-}
generatePawnMoves !position 
    | mover position == White = recurGenerateWhitePawnMoves bitboard position (emptySquaresBitboard position) bitboard []
    | otherwise = recurGenerateBlackPawnMoves bitboard position (emptySquaresBitboard position) bitboard []
    where bitboard = bitboardForMover position Pawn

recurGenerateWhitePawnMoves :: Bitboard -> Position -> Bitboard -> Bitboard -> MoveList -> MoveList
{-# INLINE recurGenerateWhitePawnMoves #-}
recurGenerateWhitePawnMoves 0 _ _ _ !result = result
recurGenerateWhitePawnMoves !fromSquares !position !emptySquares !moverPawns !result =
  recurGenerateWhitePawnMoves (xor fromSquares (bit fromSquare)) position emptySquares moverPawns (result ++ thisResult)
  where !fromSquare = countTrailingZeros fromSquares
        !pawnForwardAndCaptureMoves = pawnForwardAndCaptureMovesBitboard fromSquare whitePawnMovesCapture (pawnForwardMovesBitboard ((.&.) (whitePawnMovesForward fromSquare) emptySquares) position) position
        !thisResult = generatePawnMovesFromToSquares fromSquare pawnForwardAndCaptureMoves

recurGenerateBlackPawnMoves :: Bitboard -> Position -> Bitboard -> Bitboard -> MoveList -> MoveList
{-# INLINE recurGenerateBlackPawnMoves #-}
recurGenerateBlackPawnMoves 0 _ _ _ !result = result
recurGenerateBlackPawnMoves !fromSquares !position !emptySquares !moverPawns !result =
  recurGenerateBlackPawnMoves (xor fromSquares (bit fromSquare)) position emptySquares moverPawns (result ++ thisResult)
  where !fromSquare = countTrailingZeros fromSquares
        !pawnForwardAndCaptureMoves = pawnForwardAndCaptureMovesBitboard fromSquare blackPawnMovesCapture (pawnForwardMovesBitboard ((.&.) (blackPawnMovesForward fromSquare) emptySquares) position) position
        !thisResult = generatePawnMovesFromToSquares fromSquare pawnForwardAndCaptureMoves

pawnForwardMovesBitboard :: Bitboard -> Position -> Bitboard
{-# INLINE pawnForwardMovesBitboard #-}
pawnForwardMovesBitboard !pawnMoves !position = (.|.) pawnMoves ((.&.) (potentialPawnJumpMoves pawnMoves position) (emptySquaresBitboard position))

enPassantCaptureRank :: Mover -> Bitboard
{-# INLINE enPassantCaptureRank #-}
enPassantCaptureRank White = rank6Bits
enPassantCaptureRank Black = rank3Bits

pawnForwardAndCaptureMovesBitboard :: Square -> (Int -> Bitboard) -> Bitboard -> Position -> Bitboard
{-# INLINE pawnForwardAndCaptureMovesBitboard #-}
pawnForwardAndCaptureMovesBitboard !fromSquare !capturePawnMoves !nonCaptures !position = (.|.) nonCaptures captures
  where eps = enPassantSquare position
        !captures = if eps /= enPassantNotAvailable && (.&.) (bit eps) (enPassantCaptureRank (mover position)) /= 0
                        then pawnCapturesPlusEnPassantSquare capturePawnMoves fromSquare position
                        else pawnCaptures capturePawnMoves fromSquare (enemyBitboard position)

pawnCapturesPlusEnPassantSquare :: (Int -> Bitboard) -> Square -> Position -> Bitboard
{-# INLINE pawnCapturesPlusEnPassantSquare #-}
pawnCapturesPlusEnPassantSquare !bs !square !position = pawnCaptures bs square (enemyBitboard position .|. (if eps == enPassantNotAvailable then 0 else bit eps)) where eps = enPassantSquare position

pawnCaptures :: (Int -> Bitboard) -> Square -> Bitboard -> Bitboard
{-# INLINE pawnCaptures #-}
pawnCaptures !captureMask !square = (.&.) (captureMask square)

potentialPawnJumpMoves :: Bitboard -> Position -> Bitboard
{-# INLINE potentialPawnJumpMoves #-}
potentialPawnJumpMoves !bb !position = if mover position == White then (.&.) (bb `shiftL` 8) rank4Bits else (.&.) (bb `shiftR` 8) rank5Bits

generateCastleMoves :: Position -> MoveList
{-# INLINE generateCastleMoves #-}
generateCastleMoves !position = if mover position == White
    then [(.|.) (fromSquareMask 3) 1 :: Move | (whiteKingCastleAvailable position) && (.&.) allPieces emptyCastleSquaresWhiteKing == 0 && not (anySquaresInBitboardAttacked position Black noCheckCastleSquaresWhiteKing)] ++
         [(.|.) (fromSquareMask 3) 5 :: Move | (whiteQueenCastleAvailable position) && (.&.) allPieces emptyCastleSquaresWhiteQueen == 0 && not (anySquaresInBitboardAttacked position Black noCheckCastleSquaresWhiteQueen)]
    else [(.|.) (fromSquareMask 59) 57 :: Move | (blackKingCastleAvailable position) && (.&.) allPieces emptyCastleSquaresBlackKing == 0 && not (anySquaresInBitboardAttacked position White noCheckCastleSquaresBlackKing)] ++
         [(.|.) (fromSquareMask 59) 61 :: Move | (blackQueenCastleAvailable position) && (.&.) allPieces emptyCastleSquaresBlackQueen == 0 && not (anySquaresInBitboardAttacked position White noCheckCastleSquaresBlackQueen)]
    where allPieces = allPiecesBitboard position

anySquaresInBitboardAttacked :: Position -> Mover -> Bitboard -> Bool
{-# INLINE anySquaresInBitboardAttacked #-}
anySquaresInBitboardAttacked position attacker bitboard = any (\x -> isSquareAttackedBy position x attacker) bitRefs where !bitRefs = bitRefList bitboard

pawnMovesCaptureOfColour :: Mover -> Int -> Bitboard
{-# INLINE pawnMovesCaptureOfColour #-}
pawnMovesCaptureOfColour !mover = if mover == White then whitePawnMovesCapture else blackPawnMovesCapture

kingSquare :: Position -> Mover -> Square
{-# INLINE kingSquare #-}
kingSquare !position !colour = if colour == White
    then countTrailingZeros (whiteKingBitboard position)
    else countTrailingZeros (blackKingBitboard position)

isCheck :: Position -> Mover -> Bool
{-# INLINE isCheck #-}
isCheck !position !colour = isSquareAttackedBy position (kingSquare position colour) (if colour == White then Black else White)

magicIndexForRook :: Square -> Bitboard -> Int
{-# INLINE magicIndexForRook #-}
magicIndexForRook !pieceSquare !allPieceBitboard = fromIntegral (shiftR rawIndex shiftMagic) :: Int
    where !numberMagic = magicNumber magicRookVars pieceSquare
          !shiftMagic = magicNumberShifts magicRookVars pieceSquare
          !maskMagic = occupancyMask magicRookVars pieceSquare
          !occupancy = (.&.) allPieceBitboard maskMagic
          !rawIndex = fromIntegral(occupancy * numberMagic) :: Word

magicIndexForBishop :: Square -> Bitboard -> Int
{-# INLINE magicIndexForBishop #-}
magicIndexForBishop !pieceSquare !allPieceBitboard = fromIntegral (shiftR rawIndex shiftMagic) :: Int
    where !numberMagic = magicNumber magicBishopVars pieceSquare
          !shiftMagic = magicNumberShifts magicBishopVars pieceSquare
          !maskMagic = occupancyMask magicBishopVars pieceSquare
          !occupancy = (.&.) allPieceBitboard maskMagic
          !rawIndex = fromIntegral(occupancy * numberMagic) :: Word

rookMovePiecesBitboard :: Position -> Mover -> Bitboard
{-# INLINE rookMovePiecesBitboard #-}
rookMovePiecesBitboard !position !mover = if mover == White
    then (.|.) (whiteRookBitboard position) (whiteQueenBitboard position)
    else (.|.) (blackRookBitboard position) (blackQueenBitboard position)

bishopMovePiecesBitboard :: Position -> Mover -> Bitboard
{-# INLINE bishopMovePiecesBitboard #-}
bishopMovePiecesBitboard !position !mover = if mover == White
    then (.|.) (whiteBishopBitboard position) (whiteQueenBitboard position)
    else (.|.) (blackBishopBitboard position) (blackQueenBitboard position)

isSquareAttackedByAnyKnight :: Bitboard -> Square -> Mover -> Bool
{-# INLINE isSquareAttackedByAnyKnight #-}
isSquareAttackedByAnyKnight !knightBitboard !attackedSquare !attacker = (.&.) knightBitboard (knightMovesBitboards attackedSquare) /= 0

isSquareAttackedByKing :: Bitboard -> Square -> Mover -> Bool
{-# INLINE isSquareAttackedByKing #-}
isSquareAttackedByKing !king !attackedSquare !attacker = (.&.) king (kingMovesBitboards attackedSquare) /= 0

isSquareAttackedByAnyPawn :: Bitboard -> Bitboard -> Square -> Mover -> Bool
{-# INLINE isSquareAttackedByAnyPawn #-}
isSquareAttackedByAnyPawn !pawns !pawnAttacks !attackedSquare !attacker = (.&.) pawns pawnAttacks /= 0

isSquareAttackedByAnyBishop :: Bitboard -> Bitboard -> Square -> Bool
{-# INLINE isSquareAttackedByAnyBishop #-}
isSquareAttackedByAnyBishop !allPieces !attackingBishops !attackedSquare = any (\x -> isBishopAttackingSquare attackedSquare x allPieces) bitRefs where !bitRefs = bitRefList attackingBishops

isSquareAttackedByAnyRook :: Bitboard -> Bitboard -> Square -> Bool
{-# INLINE isSquareAttackedByAnyRook #-}
isSquareAttackedByAnyRook !allPieces !attackingRooks !attackedSquare = any (\x -> isRookAttackingSquare attackedSquare x allPieces) bitRefs where !bitRefs = bitRefList attackingRooks

isBishopAttackingSquare :: Square -> Square -> Bitboard -> Bool
{-# INLINE isBishopAttackingSquare #-}
isBishopAttackingSquare !attackedSquare !pieceSquare !allPieceBitboard = testBit (magic magicBishopVars pieceSquare (magicIndexForBishop pieceSquare allPieceBitboard)) attackedSquare

isRookAttackingSquare :: Square -> Square -> Bitboard -> Bool
{-# INLINE isRookAttackingSquare #-}
isRookAttackingSquare !attackedSquare !pieceSquare !allPieceBitboard = testBit (magic magicRookVars pieceSquare (magicIndexForRook pieceSquare allPieceBitboard)) attackedSquare
  
isSquareAttackedBy :: Position -> Square -> Mover -> Bool
{-# INLINE isSquareAttackedBy #-}
isSquareAttackedBy !position !attackedSquare !attacker =
  attackedByRook || attackedByBishop || attackedByKing || attackedByPawn || attackedByKnight
  where !allPieces = allPiecesBitboard position
        !rooks = rookMovePiecesBitboard position attacker
        !bishops = bishopMovePiecesBitboard position attacker
        !knights = (if attacker == White then whiteKnightBitboard else blackKnightBitboard) position
        !pawnAttacks = pawnMovesCaptureOfColour (switchSide attacker) attackedSquare
        !pawns = (if attacker == White then whitePawnBitboard else blackPawnBitboard) position
        !king = (if attacker == White then whiteKingBitboard else blackKingBitboard) position
        attackedByPawn = (pawns /= 0 && isSquareAttackedByAnyPawn pawns pawnAttacks attackedSquare attacker)
        attackedByKnight = (knights /= 0 && isSquareAttackedByAnyKnight knights attackedSquare attacker)
        attackedByRook = (rooks /= 0 && isSquareAttackedByAnyRook allPieces rooks attackedSquare)
        attackedByBishop = (bishops /= 0 && isSquareAttackedByAnyBishop allPieces bishops attackedSquare)
        attackedByKing = isSquareAttackedByKing king attackedSquare attacker

moves :: Position -> MoveList
{-# INLINE moves #-}
moves !position =
    par moves1 (moves1 ++ moves2)
    where moves1 = generatePawnMoves position ++ generateKingMoves position ++ generateKnightMoves position
          moves2 = generateRookMoves position ++ generateBishopMoves position ++ generateCastleMoves position

