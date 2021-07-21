{-# LANGUAGE StrictData,BangPatterns #-}

{-# OPTIONS_GHC -Wno-overflowed-literals #-}

module Search.MoveGenerator where

import Types
    ( Position(Position, whitePiecesBitboard, blackPiecesBitboard,
               enPassantSquare, mover, whiteKingCastleAvailable,
               whiteQueenCastleAvailable, blackKingCastleAvailable,
               blackQueenCastleAvailable, whiteRookBitboard, blackRookBitboard,
               whiteBishopBitboard, whiteQueenBitboard, blackBishopBitboard,
               blackQueenBitboard, whitePawnBitboard, whiteKnightBitboard,
               whiteKingBitboard, allPiecesBitboard, blackPawnBitboard,
               blackKnightBitboard, blackKingBitboard),
      Piece(Bishop, Knight, King, Pawn, Rook),
      Mover(..),
      bitboardForMover,
      sliderBitboardForColour )
import Alias ( Bitboard, Move, MoveList, Square )
import Data.Word ()
import Data.Bits
    ( Bits(testBit, complement, xor, bit, shiftL, shiftR, (.|.), (.&.),
           clearBit),
      FiniteBits(countTrailingZeros) )
import Util.Bitboards
    ( blackPawnMovesCapture,
      blackPawnMovesForward,
      emptyCastleSquaresBlackKing,
      emptyCastleSquaresBlackQueen,
      emptyCastleSquaresWhiteKing,
      emptyCastleSquaresWhiteQueen,
      emptySquaresBitboard,
      enemyBitboard,
      kingMovesBitboards,
      knightMovesBitboards,
      noCheckCastleSquaresBlackKing,
      noCheckCastleSquaresBlackQueen,
      noCheckCastleSquaresWhiteKing,
      noCheckCastleSquaresWhiteQueen,
      rank3Bits,
      rank4Bits,
      rank5Bits,
      rank6Bits,
      whitePawnMovesCapture,
      whitePawnMovesForward )
import Util.MagicBitboards
    ( magic,
      magicBishopVars,
      magicRookVars,
      MagicVars(magicNumber, magicNumberShifts, occupancyMask) )
import Util.Utils ( fromSquareMask )
import Search.MoveConstants
    ( enPassantNotAvailable,
      promotionBishopMoveMask,
      promotionKnightMoveMask,
      promotionQueenMoveMask,
      promotionRookMoveMask )

{-# INLINE allBitsExceptFriendlyPieces #-}
allBitsExceptFriendlyPieces :: Position -> Bitboard
allBitsExceptFriendlyPieces !position = complement (if mover position == White then whitePiecesBitboard position else blackPiecesBitboard position)

{-# INLINE movesFromToSquaresBitboard #-}
movesFromToSquaresBitboard :: Square -> Bitboard -> MoveList
movesFromToSquaresBitboard !fromSquare !toSquares = go (fromSquareMask fromSquare) toSquares []
    where
        go :: Square -> Bitboard -> MoveList -> MoveList
        go _ 0 !result = result
        go !fromSquare !toSquares !result = 
            go fromSquare (clearBit toSquares square) ((.|.) fromSquare square : result)
            where !square = countTrailingZeros toSquares

{-# INLINE generateKnightMoves #-}
{-# SCC generateKnightMoves #-}
generateKnightMoves :: Position -> MoveList
generateKnightMoves !position = go position (bitboardForMover position Knight) []
    where
        go :: Position -> Bitboard -> MoveList -> MoveList
        go _ 0 !result = result
        go !position fromSquares !result =
            go position (clearBit fromSquares fromSquare) (result ++ movesFromToSquaresBitboard fromSquare ((.&.) (knightMovesBitboards fromSquare) (allBitsExceptFriendlyPieces position)))
            where !fromSquare = countTrailingZeros fromSquares

{-# INLINE generateKingMoves #-}
generateKingMoves :: Position -> MoveList
generateKingMoves !position =
    movesFromToSquaresBitboard kingSquare ((.&.) (kingMovesBitboards kingSquare) (allBitsExceptFriendlyPieces position))
        where !kingSquare = countTrailingZeros (bitboardForMover position King)

generateSliderMoves :: Position -> Piece -> MoveList
generateSliderMoves !position !piece = go bitboard position magicVars []
    where !magicVars = if piece == Bishop then magicBishopVars else magicRookVars
          !thisMover = mover position
          !bitboard = sliderBitboardForColour position thisMover piece

          go :: Bitboard -> Position -> MagicVars -> MoveList -> MoveList
          go 0 _ _ !result = result
          go fromSquares !position !magicVars !result = go (clearBit fromSquares fromSquare) position magicVars (result ++ thisResult)
              where !numberMagic = magicNumber magicVars fromSquare
                    !shiftMagic = magicNumberShifts magicVars fromSquare
                    !maskMagic = occupancyMask magicVars fromSquare
                    !occupancy = (.&.) (allPiecesBitboard position) maskMagic
                    !rawIndex = fromIntegral (occupancy * numberMagic) :: Word
                    !toSquaresMagicIndex = fromIntegral(shiftR rawIndex shiftMagic) :: Int
                    !toSquaresBitboard = (.&.) (magic magicVars fromSquare toSquaresMagicIndex) (allBitsExceptFriendlyPieces position)
                    !fromSquare = countTrailingZeros fromSquares
                    !thisResult = recurGenerateSliderMovesWithToSquares (fromSquareMask fromSquare) toSquaresBitboard []

recurGenerateSliderMovesWithToSquares :: Square -> Bitboard -> MoveList -> MoveList
recurGenerateSliderMovesWithToSquares !fromSquare 0 !result = result
recurGenerateSliderMovesWithToSquares !fromSquare !toSquares !result =
    recurGenerateSliderMovesWithToSquares fromSquare (clearBit toSquares square) ((.|.) fromSquare square : result)
    where !square = countTrailingZeros toSquares

{-# INLINE promotionMoves #-}
promotionMoves :: Move -> MoveList
promotionMoves !move = [(.|.) move promotionQueenMoveMask, (.|.) move promotionRookMoveMask, (.|.) move promotionBishopMoveMask, (.|.) move promotionKnightMoveMask]

{-# INLINE generatePawnMovesFromToSquares #-}
generatePawnMovesFromToSquares :: Square -> Bitboard -> MoveList
generatePawnMovesFromToSquares !fromSquare !toSquares = recurGeneratePawnMovesFromToSquares (fromSquareMask fromSquare) toSquares []

recurGeneratePawnMovesFromToSquares :: Move -> Bitboard -> MoveList -> MoveList
recurGeneratePawnMovesFromToSquares _ 0 !result = result
recurGeneratePawnMovesFromToSquares !mask !toSquares !result = recurGeneratePawnMovesFromToSquares mask (xor toSquares (bit thisToSquare)) newResult
  where !thisToSquare = countTrailingZeros toSquares
        !baseMove = (.|.) mask thisToSquare
        !newResult = if thisToSquare >= 56 || thisToSquare <= 7
                        then promotionMoves baseMove ++ result
                        else baseMove : result

{-# INLINE generatePawnMoves #-}
generatePawnMoves :: Position -> MoveList
generatePawnMoves !position 
    | mover position == White = recurGenerateWhitePawnMoves bitboard position (emptySquaresBitboard position) bitboard []
    | otherwise               = recurGenerateBlackPawnMoves bitboard position (emptySquaresBitboard position) bitboard []
    where 
        bitboard = bitboardForMover position Pawn

recurGenerateWhitePawnMoves :: Bitboard -> Position -> Bitboard -> Bitboard -> MoveList -> MoveList
recurGenerateWhitePawnMoves 0 _ _ _ !result = result
recurGenerateWhitePawnMoves !fromSquares !position !emptySquares !moverPawns !result =
  recurGenerateWhitePawnMoves (clearBit fromSquares fromSquare) position emptySquares moverPawns (result ++ thisResult)
  where !fromSquare = countTrailingZeros fromSquares
        !pawnForwardAndCaptureMoves = pawnForwardAndCaptureMovesBitboard fromSquare whitePawnMovesCapture (pawnForwardMovesBitboard ((.&.) (whitePawnMovesForward fromSquare) emptySquares) position) position
        !thisResult = generatePawnMovesFromToSquares fromSquare pawnForwardAndCaptureMoves

recurGenerateBlackPawnMoves :: Bitboard -> Position -> Bitboard -> Bitboard -> MoveList -> MoveList
recurGenerateBlackPawnMoves 0 _ _ _ !result = result
recurGenerateBlackPawnMoves !fromSquares !position !emptySquares !moverPawns !result =
  recurGenerateBlackPawnMoves (clearBit fromSquares fromSquare) position emptySquares moverPawns (result ++ thisResult)
  where !fromSquare = countTrailingZeros fromSquares
        !pawnForwardAndCaptureMoves = pawnForwardAndCaptureMovesBitboard fromSquare blackPawnMovesCapture (pawnForwardMovesBitboard ((.&.) (blackPawnMovesForward fromSquare) emptySquares) position) position
        !thisResult = generatePawnMovesFromToSquares fromSquare pawnForwardAndCaptureMoves

{-# INLINE pawnForwardMovesBitboard #-}
pawnForwardMovesBitboard :: Bitboard -> Position -> Bitboard
pawnForwardMovesBitboard !pawnMoves !position = (.|.) pawnMoves ((.&.) (potentialPawnJumpMoves pawnMoves position) (emptySquaresBitboard position))

{-# INLINE enPassantCaptureRank #-}
enPassantCaptureRank :: Mover -> Bitboard
enPassantCaptureRank White = rank6Bits
enPassantCaptureRank Black = rank3Bits

{-# INLINE pawnForwardAndCaptureMovesBitboard #-}
pawnForwardAndCaptureMovesBitboard :: Square -> (Int -> Bitboard) -> Bitboard -> Position -> Bitboard
pawnForwardAndCaptureMovesBitboard !fromSquare !capturePawnMoves !nonCaptures !position = (.|.) nonCaptures captures
  where !eps = enPassantSquare position
        !captures = if eps /= enPassantNotAvailable && (.&.) (bit eps) (enPassantCaptureRank (mover position)) /= 0
                        then pawnCapturesPlusEnPassantSquare capturePawnMoves fromSquare position
                        else pawnCaptures capturePawnMoves fromSquare (enemyBitboard position)

{-# INLINE pawnCapturesPlusEnPassantSquare #-}
pawnCapturesPlusEnPassantSquare :: (Int -> Bitboard) -> Square -> Position -> Bitboard
pawnCapturesPlusEnPassantSquare !bs !square !position = pawnCaptures bs square (enemyBitboard position .|. (if eps == enPassantNotAvailable then 0 else bit eps)) where eps = enPassantSquare position

{-# INLINE pawnCaptures #-}
pawnCaptures :: (Int -> Bitboard) -> Square -> Bitboard -> Bitboard
pawnCaptures !captureMask !square = (.&.) (captureMask square)

{-# INLINE potentialPawnJumpMoves #-}
potentialPawnJumpMoves :: Bitboard -> Position -> Bitboard
potentialPawnJumpMoves !bb Position{mover=White} = (.&.) (bb `shiftL` 8) rank4Bits
potentialPawnJumpMoves !bb Position{mover=Black} = (.&.) (bb `shiftR` 8) rank5Bits

{-# INLINE generateCastleMoves #-}
generateCastleMoves :: Position -> MoveList
generateCastleMoves !position = if mover position == White
    then [(.|.) (fromSquareMask 3) 1 :: Move | whiteKingCastleAvailable position && (.&.) allPieces emptyCastleSquaresWhiteKing == 0 && not (anySquaresInBitboardAttacked position Black noCheckCastleSquaresWhiteKing)] ++
         [(.|.) (fromSquareMask 3) 5 :: Move | whiteQueenCastleAvailable position && (.&.) allPieces emptyCastleSquaresWhiteQueen == 0 && not (anySquaresInBitboardAttacked position Black noCheckCastleSquaresWhiteQueen)]
    else [(.|.) (fromSquareMask 59) 57 :: Move | blackKingCastleAvailable position && (.&.) allPieces emptyCastleSquaresBlackKing == 0 && not (anySquaresInBitboardAttacked position White noCheckCastleSquaresBlackKing)] ++
         [(.|.) (fromSquareMask 59) 61 :: Move | blackQueenCastleAvailable position && (.&.) allPieces emptyCastleSquaresBlackQueen == 0 && not (anySquaresInBitboardAttacked position White noCheckCastleSquaresBlackQueen)]
    where !allPieces = allPiecesBitboard position

anySquaresInBitboardAttacked :: Position -> Mover -> Bitboard -> Bool
anySquaresInBitboardAttacked _ _ 0 = False
anySquaresInBitboardAttacked !position !attacker bitboard =
    isSquareAttackedBy position square attacker || anySquaresInBitboardAttacked position attacker (clearBit bitboard square)
        where square = countTrailingZeros bitboard

{-# INLINE pawnMovesCaptureOfColour #-}
pawnMovesCaptureOfColour :: Mover -> Int -> Bitboard
pawnMovesCaptureOfColour White = whitePawnMovesCapture 
pawnMovesCaptureOfColour Black = blackPawnMovesCapture

{-# INLINE kingSquare #-}
kingSquare :: Position -> Mover -> Square
kingSquare !position White = countTrailingZeros (whiteKingBitboard position)
kingSquare !position Black = countTrailingZeros (blackKingBitboard position)

{-# INLINE isCheck #-}
isCheck :: Position -> Mover -> Bool
isCheck !position White = isSquareAttackedBy position (kingSquare position White) Black
isCheck !position Black = isSquareAttackedBy position (kingSquare position Black) White

{-# INLINE magicIndexForRook #-}
magicIndexForRook :: Square -> Bitboard -> Int
magicIndexForRook !pieceSquare !allPieceBitboard = fromIntegral (shiftR rawIndex shiftMagic) :: Int
    where !numberMagic = magicNumber magicRookVars pieceSquare
          !shiftMagic = magicNumberShifts magicRookVars pieceSquare
          !maskMagic = occupancyMask magicRookVars pieceSquare
          !occupancy = (.&.) allPieceBitboard maskMagic
          !rawIndex = fromIntegral(occupancy * numberMagic) :: Word

{-# INLINE magicIndexForBishop #-}
magicIndexForBishop :: Square -> Bitboard -> Int
magicIndexForBishop !pieceSquare !allPieceBitboard = fromIntegral (shiftR rawIndex shiftMagic) :: Int
    where !numberMagic = magicNumber magicBishopVars pieceSquare
          !shiftMagic = magicNumberShifts magicBishopVars pieceSquare
          !maskMagic = occupancyMask magicBishopVars pieceSquare
          !occupancy = (.&.) allPieceBitboard maskMagic
          !rawIndex = fromIntegral(occupancy * numberMagic) :: Word

{-# INLINE rookMovePiecesBitboard #-}
rookMovePiecesBitboard :: Position -> Mover -> Bitboard
rookMovePiecesBitboard !position White = (.|.) (whiteRookBitboard position) (whiteQueenBitboard position)
rookMovePiecesBitboard !position Black = (.|.) (blackRookBitboard position) (blackQueenBitboard position)

{-# INLINE bishopMovePiecesBitboard #-}
bishopMovePiecesBitboard :: Position -> Mover -> Bitboard
bishopMovePiecesBitboard !position White = (.|.) (whiteBishopBitboard position) (whiteQueenBitboard position)
bishopMovePiecesBitboard !position Black = (.|.) (blackBishopBitboard position) (blackQueenBitboard position)

{-# INLINE isSquareAttackedByAnyKnight #-}
isSquareAttackedByAnyKnight :: Bitboard -> Square -> Bool
isSquareAttackedByAnyKnight 0 _ = False
isSquareAttackedByAnyKnight !knightBitboard !attackedSquare = (.&.) knightBitboard (knightMovesBitboards attackedSquare) /= 0

{-# INLINE isSquareAttackedByKing #-}
isSquareAttackedByKing :: Bitboard -> Square -> Bool
isSquareAttackedByKing !king !attackedSquare = (.&.) king (kingMovesBitboards attackedSquare) /= 0

{-# INLINE isSquareAttackedByAnyPawn #-}
isSquareAttackedByAnyPawn :: Bitboard -> Bitboard -> Square -> Bool
isSquareAttackedByAnyPawn 0 _ _ = False
isSquareAttackedByAnyPawn !pawns !pawnAttacks !attackedSquare = (.&.) pawns pawnAttacks /= 0

{-# INLINE isSquareAttackedByAnyBishop #-}
isSquareAttackedByAnyBishop :: Bitboard -> Bitboard -> Square -> Bool
isSquareAttackedByAnyBishop _ 0 _ = False
isSquareAttackedByAnyBishop !allPieces !attackingBishops !attackedSquare =
    isBishopAttackingSquare attackedSquare bishopSquare allPieces ||
    isSquareAttackedByAnyBishop allPieces (clearBit attackingBishops bishopSquare) attackedSquare
        where bishopSquare = countTrailingZeros attackingBishops

{-# INLINE isSquareAttackedByAnyRook #-}
isSquareAttackedByAnyRook :: Bitboard -> Bitboard -> Square -> Bool
isSquareAttackedByAnyRook _ 0 _ = False
isSquareAttackedByAnyRook !allPieces !attackingRooks !attackedSquare =
    isRookAttackingSquare attackedSquare rookSquare allPieces ||
    isSquareAttackedByAnyRook allPieces (clearBit attackingRooks rookSquare) attackedSquare
        where rookSquare = countTrailingZeros attackingRooks

{-# INLINE isBishopAttackingSquare #-}
isBishopAttackingSquare :: Square -> Square -> Bitboard -> Bool
isBishopAttackingSquare !attackedSquare !pieceSquare !allPieceBitboard =
    testBit (magic magicBishopVars pieceSquare (magicIndexForBishop pieceSquare allPieceBitboard)) attackedSquare

{-# INLINE isRookAttackingSquare #-}
isRookAttackingSquare :: Square -> Square -> Bitboard -> Bool
isRookAttackingSquare !attackedSquare !pieceSquare !allPieceBitboard = 
    testBit (magic magicRookVars pieceSquare (magicIndexForRook pieceSquare allPieceBitboard)) attackedSquare

{-# INLINE isSquareAttackedBy #-}
isSquareAttackedBy :: Position -> Square -> Mover -> Bool

isSquareAttackedBy !position !attackedSquare White =
  attackedByRook || attackedByBishop || attackedByKing || attackedByPawn || attackedByKnight
  where !allPieces       = allPiecesBitboard position
        attackedByPawn   = isSquareAttackedByAnyPawn (whitePawnBitboard position) (pawnMovesCaptureOfColour Black attackedSquare) attackedSquare
        attackedByKnight = isSquareAttackedByAnyKnight (whiteKnightBitboard position) attackedSquare
        attackedByRook   = isSquareAttackedByAnyRook allPieces (rookMovePiecesBitboard position White) attackedSquare
        attackedByBishop = isSquareAttackedByAnyBishop allPieces (bishopMovePiecesBitboard position White) attackedSquare
        attackedByKing   = isSquareAttackedByKing (whiteKingBitboard position) attackedSquare

isSquareAttackedBy !position !attackedSquare Black =
  attackedByRook || attackedByBishop || attackedByKing || attackedByPawn || attackedByKnight
  where !allPieces       = allPiecesBitboard position
        attackedByPawn   = isSquareAttackedByAnyPawn (blackPawnBitboard position) (pawnMovesCaptureOfColour White attackedSquare) attackedSquare
        attackedByKnight = isSquareAttackedByAnyKnight (blackKnightBitboard position) attackedSquare
        attackedByRook   = isSquareAttackedByAnyRook allPieces (rookMovePiecesBitboard position Black) attackedSquare
        attackedByBishop = isSquareAttackedByAnyBishop allPieces (bishopMovePiecesBitboard position Black) attackedSquare
        attackedByKing   = isSquareAttackedByKing (blackKingBitboard position) attackedSquare

{-# INLINE moves #-}
moves :: Position -> MoveList
moves !position = generatePawnMoves position ++ 
                  generateCastleMoves position ++ 
                  generateKnightMoves position ++ 
                  generateSliderMoves position Rook ++ 
                  generateSliderMoves position Bishop ++ 
                  generateKingMoves position

