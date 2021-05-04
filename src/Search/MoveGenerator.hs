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

bitboardForMover :: Position -> Piece -> Bitboard
bitboardForMover !position = bitboardForColour position (mover position)

bitboardForColour :: Position -> Mover -> Piece -> Bitboard
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
bitRefList !bitboard = if popCount bitboard == 1 then [countTrailingZeros bitboard] else recurBitRefList bitboard []

recurBitRefList :: Bitboard -> [Square] -> [Square]
recurBitRefList 0 !result = result
recurBitRefList !bitboard !result = recurBitRefList (xor bitboard (bit square)) (square : result) where square = countTrailingZeros bitboard

allBitsExceptFriendlyPieces :: Position -> Bitboard
allBitsExceptFriendlyPieces !position = complement (if mover position == White then whitePiecesBitboard position else blackPiecesBitboard position)

movesFromToSquaresBitboard :: Square -> Bitboard -> MoveList
movesFromToSquaresBitboard !fromSquare !toSquares = recurMovesFromToSquaresBitboard (shiftL fromSquare 16) toSquares []

recurMovesFromToSquaresBitboard :: Square -> Bitboard -> MoveList -> MoveList
recurMovesFromToSquaresBitboard _ 0 !result = result
recurMovesFromToSquaresBitboard !fromSquare !toSquares !result = 
    recurMovesFromToSquaresBitboard fromSquare (xor toSquares (bit square)) ((.|.) fromSquare square : result)
    where square = countTrailingZeros toSquares

generateKnightMoves :: Position -> MoveList
generateKnightMoves !position = recurKnightMoves position (bitRefList (bitboardForMover position Knight)) []

recurKnightMoves :: Position -> [Square] -> MoveList -> MoveList
recurKnightMoves _ [] !result = result
recurKnightMoves !position !fromSquares !result =
    recurKnightMoves position (tail fromSquares) (result ++ movesFromToSquaresBitboard fromSquare ((.&.) (knightMovesBitboards ! fromSquare) (allBitsExceptFriendlyPieces position)))
        where fromSquare = head fromSquares

generateKingMoves :: Position -> MoveList
generateKingMoves !position =
    movesFromToSquaresBitboard kingSquare ((.&.) (kingMovesBitboards ! kingSquare) (allBitsExceptFriendlyPieces position))
        where kingSquare = countTrailingZeros (bitboardForMover position King)

generateBishopMoves :: Position -> MoveList
generateBishopMoves position = generateSliderMoves position Bishop

generateRookMoves :: Position -> MoveList
generateRookMoves !position = generateSliderMoves position Rook

generateSliderMoves :: Position -> Piece -> MoveList
generateSliderMoves !position !piece = recurGenerateSliderMoves fromSquares position magicVars []
    where bitboards = position
          magicVars = if piece == Bishop then magicBishopVars else magicRookVars
          thisMover = mover position
          bitboard = bitboardForColour bitboards thisMover piece .|. bitboardForColour bitboards thisMover Queen
          fromSquares = bitRefList bitboard

recurGenerateSliderMoves :: [Square] -> Position -> MagicVars -> MoveList -> MoveList
recurGenerateSliderMoves [] _ _ !result = result
recurGenerateSliderMoves !fromSquares !position !magicVars !result =
  recurGenerateSliderMoves (tail fromSquares) position magicVars (result ++ thisResult)
    where fromSquare = head fromSquares
          numberMagic = magicNumber magicVars fromSquare
          shiftMagic = magicNumberShifts magicVars fromSquare
          maskMagic = occupancyMask magicVars fromSquare
          occupancy = (.&.) (allPiecesBitboard position) maskMagic
          rawIndex = fromIntegral (occupancy * numberMagic) :: Word
          toSquaresMagicIndex = fromIntegral(shiftR rawIndex shiftMagic) :: Int
          toSquaresBitboard = (.&.) (magic magicVars fromSquare toSquaresMagicIndex) (allBitsExceptFriendlyPieces position)
          thisResult = recurGenerateSliderMovesWithToSquares (fromSquareMask fromSquare) toSquaresBitboard []

recurGenerateSliderMovesWithToSquares :: Square -> Bitboard -> MoveList -> MoveList
recurGenerateSliderMovesWithToSquares !fromSquare 0 !result = result
recurGenerateSliderMovesWithToSquares !fromSquare !toSquares !result =
    recurGenerateSliderMovesWithToSquares fromSquare (xor toSquares (bit square)) ((.|.) fromSquare square : result)
    where square = countTrailingZeros toSquares

promotionMoves :: Move -> MoveList
promotionMoves !move = [
    (.|.) move promotionQueenMoveMask
  , (.|.) move promotionRookMoveMask
  , (.|.) move promotionBishopMoveMask
  , (.|.) move promotionKnightMoveMask]

generatePawnMovesFromToSquares :: Square -> Bitboard -> MoveList
generatePawnMovesFromToSquares !fromSquare !toSquares = recurGeneratePawnMovesFromToSquares (fromSquareMask fromSquare) toSquares []

recurGeneratePawnMovesFromToSquares :: Move -> Bitboard -> MoveList -> MoveList
recurGeneratePawnMovesFromToSquares _ 0 !result = result
recurGeneratePawnMovesFromToSquares !mask !toSquares !result = recurGeneratePawnMovesFromToSquares mask (xor toSquares (bit thisToSquare)) newResult
  where thisToSquare = countTrailingZeros toSquares
        baseMove = (.|.) mask thisToSquare
        newResult = if thisToSquare >= 56 || thisToSquare <= 7
                    then promotionMoves baseMove ++ result
                    else baseMove : result

generatePawnMoves :: Position -> MoveList
generatePawnMoves !position = recurGeneratePawnMoves bitboard position forwardPawnMoves capturePawnMoves (emptySquaresBitboard position) bitboard []
  where bitboard = bitboardForMover position Pawn
        forwardPawnMoves = if mover position == White then whitePawnMovesForward else blackPawnMovesForward
        capturePawnMoves = if mover position == White then whitePawnMovesCapture else blackPawnMovesCapture

recurGeneratePawnMoves :: Bitboard -> Position -> BitboardArray -> BitboardArray -> Bitboard -> Bitboard -> MoveList -> MoveList
recurGeneratePawnMoves 0 _ _ _ _ _ !result = result
recurGeneratePawnMoves !fromSquares !position !forwardPawnMoves !capturePawnMoves !emptySquares !moverPawns !result =
  recurGeneratePawnMoves (xor fromSquares (bit fromSquare)) position forwardPawnMoves capturePawnMoves emptySquares moverPawns (result ++ thisResult)
  where fromSquare = countTrailingZeros fromSquares
        pawnForwardAndCaptureMoves = pawnForwardAndCaptureMovesBitboard fromSquare capturePawnMoves (pawnForwardMovesBitboard ((.&.) (forwardPawnMoves ! fromSquare) emptySquares) position) position
        thisResult = generatePawnMovesFromToSquares fromSquare pawnForwardAndCaptureMoves

pawnForwardMovesBitboard :: Bitboard -> Position -> Bitboard
pawnForwardMovesBitboard !pawnMoves !position = (.|.) pawnMoves ((.&.) (potentialPawnJumpMoves pawnMoves position) (emptySquaresBitboard position))

enPassantCaptureRank :: Mover -> Bitboard
enPassantCaptureRank White = rank6Bits
enPassantCaptureRank Black = rank3Bits

pawnForwardAndCaptureMovesBitboard :: Square -> BitboardArray -> Bitboard -> Position -> Bitboard
{-# INLINE pawnForwardAndCaptureMovesBitboard #-}
pawnForwardAndCaptureMovesBitboard !fromSquare !capturePawnMoves !nonCaptures !position = (.|.) nonCaptures captures
  where eps = enPassantSquare position
        captures = if eps /= enPassantNotAvailable && (.&.) (bit eps) (enPassantCaptureRank (mover position)) /= 0
                  then pawnCapturesPlusEnPassantSquare capturePawnMoves fromSquare position
                  else pawnCaptures capturePawnMoves fromSquare (enemyBitboard position)

pawnCapturesPlusEnPassantSquare :: BitboardArray -> Square -> Position -> Bitboard
{-# INLINE pawnCapturesPlusEnPassantSquare #-}
pawnCapturesPlusEnPassantSquare !bs !square !position = pawnCaptures bs square (enemyBitboard position .|. (if eps == enPassantNotAvailable then 0 else bit eps)) where eps = enPassantSquare position

pawnCaptures :: BitboardArray -> Square -> Bitboard -> Bitboard
{-# INLINE pawnCaptures #-}
pawnCaptures !captureMask !square = (.&.) (captureMask ! square)

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
anySquaresInBitboardAttacked position attacker bitboard = any (\x -> isSquareAttackedBy position x attacker) (bitRefList bitboard)

pawnMovesCaptureOfColour :: Mover -> BitboardArray
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
    where numberMagic = magicNumber magicRookVars pieceSquare
          shiftMagic = magicNumberShifts magicRookVars pieceSquare
          maskMagic = occupancyMask magicRookVars pieceSquare
          occupancy = (.&.) allPieceBitboard maskMagic
          rawIndex = fromIntegral(occupancy * numberMagic) :: Word

magicIndexForBishop :: Square -> Bitboard -> Int
{-# INLINE magicIndexForBishop #-}
magicIndexForBishop !pieceSquare !allPieceBitboard = fromIntegral (shiftR rawIndex shiftMagic) :: Int
    where numberMagic = magicNumber magicBishopVars pieceSquare
          shiftMagic = magicNumberShifts magicBishopVars pieceSquare
          maskMagic = occupancyMask magicBishopVars pieceSquare
          occupancy = (.&.) allPieceBitboard maskMagic
          rawIndex = fromIntegral(occupancy * numberMagic) :: Word

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

isSquareAttackedByAnyKnight :: Position -> Square -> Mover -> Bool
{-# INLINE isSquareAttackedByAnyKnight #-}
isSquareAttackedByAnyKnight !position !attackedSquare !attacker = (.&.) knightBitboard (knightMovesBitboards ! attackedSquare) /= 0
    where knightBitboard = (if attacker == White then whiteKnightBitboard else blackKnightBitboard) position

isSquareAttackedByKing :: Position -> Square -> Mover -> Bool
{-# INLINE isSquareAttackedByKing #-}
isSquareAttackedByKing !position !attackedSquare !attacker = (.&.) kingBitboard (kingMovesBitboards ! attackedSquare) /= 0
    where kingBitboard = (if attacker == White then whiteKingBitboard else blackKingBitboard) position

isSquareAttackedByAnyPawn :: Position -> Square -> Mover -> Bool
{-# INLINE isSquareAttackedByAnyPawn #-}
isSquareAttackedByAnyPawn !position !attackedSquare !attacker = (.&.) pawnBitboard (pawnMovesCaptureOfColour defenderColour ! attackedSquare) /= 0
    where pawnBitboard = (if attacker == White then whitePawnBitboard else blackPawnBitboard) position
          defenderColour = switchSide attacker

isSquareAttackedByAnyBishop :: Position -> Square -> Mover -> Bool
{-# INLINE isSquareAttackedByAnyBishop #-}
isSquareAttackedByAnyBishop !position !attackedSquare !attacker = any (\x -> isBishopAttackingSquare attackedSquare x (allPiecesBitboard position)) (bitRefList (bishopMovePiecesBitboard position attacker))

isSquareAttackedByAnyRook :: Position -> Square -> Mover -> Bool
{-# INLINE isSquareAttackedByAnyRook #-}
isSquareAttackedByAnyRook !position !attackedSquare !attacker = any (\x -> isRookAttackingSquare attackedSquare x (allPiecesBitboard position)) (bitRefList (rookMovePiecesBitboard position attacker))

isBishopAttackingSquare :: Square -> Square -> Bitboard -> Bool
{-# INLINE isBishopAttackingSquare #-}
isBishopAttackingSquare !attackedSquare !pieceSquare !allPieceBitboard = (.&.) (magic magicBishopVars pieceSquare (magicIndexForBishop pieceSquare allPieceBitboard)) (bit attackedSquare) /= 0

isRookAttackingSquare :: Square -> Square -> Bitboard -> Bool
{-# INLINE isRookAttackingSquare #-}
isRookAttackingSquare !attackedSquare !pieceSquare !allPieceBitboard = (.&.) (magic magicRookVars pieceSquare (magicIndexForRook pieceSquare allPieceBitboard)) (bit attackedSquare) /= 0
  
isSquareAttackedBy :: Position -> Square -> Mover -> Bool
isSquareAttackedBy !position !attackedSquare !attacker =
  isSquareAttackedByAnyPawn position attackedSquare attacker ||
  isSquareAttackedByAnyKnight position attackedSquare attacker ||
  isSquareAttackedByKing position attackedSquare attacker ||
  isSquareAttackedByAnyRook position attackedSquare attacker ||
  isSquareAttackedByAnyBishop position attackedSquare attacker

moves :: Position -> [Move]
moves !position = 
    generatePawnMoves position ++
    generateKnightMoves position ++
    generateBishopMoves position ++
    generateRookMoves position ++
    generateKingMoves position ++
    generateCastleMoves position 

