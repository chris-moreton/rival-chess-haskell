{-# LANGUAGE StrictData,BangPatterns #-}

{-# OPTIONS_GHC -Wno-overflowed-literals #-}

module Search.MoveGenerator where

import Types
import Data.Word
import Data.Bits
import Util.Bitboards
import Util.MagicBitboards
import Util.Utils
import Search.MoveConstants
import Data.Array.IArray
import qualified Data.DList as DList
import qualified Data.Vector.Storable as V

bitboardForMover :: Position -> Piece -> Bitboard
bitboardForMover position = bitboardForColour position (mover position)

bitboardForColour :: Position -> Mover -> Piece -> Bitboard
bitboardForColour pieceBitboards White King = whiteKingBitboard pieceBitboards
bitboardForColour pieceBitboards White Queen = whiteQueenBitboard pieceBitboards
bitboardForColour pieceBitboards White Rook = whiteRookBitboard pieceBitboards
bitboardForColour pieceBitboards White Knight = whiteKnightBitboard pieceBitboards
bitboardForColour pieceBitboards White Bishop = whiteBishopBitboard pieceBitboards
bitboardForColour pieceBitboards White Pawn = whitePawnBitboard pieceBitboards
bitboardForColour pieceBitboards Black King = blackKingBitboard pieceBitboards
bitboardForColour pieceBitboards Black Queen = blackQueenBitboard pieceBitboards
bitboardForColour pieceBitboards Black Rook = blackRookBitboard pieceBitboards
bitboardForColour pieceBitboards Black Knight = blackKnightBitboard pieceBitboards
bitboardForColour pieceBitboards Black Bishop = blackBishopBitboard pieceBitboards
bitboardForColour pieceBitboards Black Pawn = blackPawnBitboard pieceBitboards

bitRefList :: Bitboard -> [Square]
bitRefList !bitboard = recurBitRefList bitboard []

recurBitRefList :: Bitboard -> [Square] -> [Square]
recurBitRefList 0 !result = result
recurBitRefList !bitboard !result = recurBitRefList (xor bitboard (bit square)) (square : result) where square = countTrailingZeros bitboard

allBitsExceptFriendlyPieces :: Position -> Bitboard
allBitsExceptFriendlyPieces !position = complement (foldl (.|.) 0 (bitboardListForColour position (mover position)))

movesFromToSquares :: Square -> [Square] -> MoveList
movesFromToSquares !fromSquare !toSquares = recurMovesFromToSquares fromSquare toSquares []

recurMovesFromToSquares :: Square -> [Square] -> [Move] -> MoveList
recurMovesFromToSquares _ [] !result = DList.fromList result
recurMovesFromToSquares !fromSquare !toSquares !result =
  recurMovesFromToSquares fromSquare (tail toSquares) ((.|.) shiftedFrom (head toSquares) : result)
    where shiftedFrom = shiftL fromSquare 16

generateKnightMoves :: Position -> MoveList
generateKnightMoves !position = recurKnightMoves position (bitRefList (bitboardForMover position Knight)) DList.empty

recurKnightMoves :: Position -> [Square] -> MoveList -> MoveList
recurKnightMoves _ [] !result = result
recurKnightMoves !position !fromSquares !result =
    recurKnightMoves position (tail fromSquares) (result `DList.append` movesFromToSquares fromSquare toSquares)
        where fromSquare = head fromSquares
              toSquares = bitRefList ((.&.) (knightMovesBitboards V.! fromSquare) (allBitsExceptFriendlyPieces position))

generateKingMoves :: Position -> MoveList
generateKingMoves !position =
    movesFromToSquares kingSquare toSquares
        where kingSquare = countTrailingZeros (bitboardForMover position King)
              toSquares = bitRefList ((.&.) (kingMovesBitboards V.! kingSquare) (allBitsExceptFriendlyPieces position))

generateBishopMoves :: Position -> MoveList
generateBishopMoves position = generateSliderMoves position Bishop

generateRookMoves :: Position -> MoveList
generateRookMoves !position = generateSliderMoves position Rook

generateSliderMoves :: Position -> Piece -> MoveList
generateSliderMoves !position !piece = recurGenerateSliderMoves fromSquares position magicVars DList.empty
    where bitboards = position
          magicVars = if piece == Bishop then magicBishopVars else magicRookVars
          thisMover = mover position
          bitboard = bitboardForColour bitboards thisMover piece .|. bitboardForColour bitboards thisMover Queen
          fromSquares = bitRefList bitboard

recurGenerateSliderMoves :: [Square] -> Position -> MagicVars -> MoveList -> MoveList
recurGenerateSliderMoves [] _ _ !result = result
recurGenerateSliderMoves !fromSquares !position !magicVars !result =
  recurGenerateSliderMoves (tail fromSquares) position magicVars (result `DList.append` thisResult)
    where fromSquare = head fromSquares
          moveMagic = magicMoves magicVars ! fromSquare
          numberMagic = magicNumber magicVars V.! fromSquare
          shiftMagic = magicNumberShifts magicVars V.! fromSquare
          maskMagic = occupancyMask magicVars V.! fromSquare
          occupancy = (.&.) (allPiecesBitboard position) maskMagic
          rawIndex = fromIntegral (occupancy * numberMagic) :: Word
          toSquaresMagicIndex = fromIntegral(shiftR rawIndex shiftMagic) :: Int
          toSquaresBitboard = (.&.) (moveMagic V.! toSquaresMagicIndex) (allBitsExceptFriendlyPieces position)
          toSquares = bitRefList toSquaresBitboard
          thisResult = recurGenerateSliderMovesWithToSquares fromSquare toSquares []

recurGenerateSliderMovesWithToSquares :: Square -> [Square] -> [Move] -> MoveList
recurGenerateSliderMovesWithToSquares !fromSquare [] !result = DList.fromList result
recurGenerateSliderMovesWithToSquares !fromSquare !toSquares !result =
  recurGenerateSliderMovesWithToSquares fromSquare (tail toSquares) ((.|.) (fromSquareMask fromSquare) (head toSquares) : result)

promotionMoves :: Move -> MoveList
promotionMoves !move = DList.fromList ([
    (.|.) move promotionQueenMoveMask
  , (.|.) move promotionRookMoveMask
  , (.|.) move promotionBishopMoveMask
  , (.|.) move promotionKnightMoveMask] :: [Move])

generatePawnMovesFromToSquares :: Square -> [Square] -> MoveList
generatePawnMovesFromToSquares !fromSquare !toSquares = recurGeneratePawnMovesFromToSquares mask toSquares DList.empty
  where mask = fromSquareMask fromSquare

recurGeneratePawnMovesFromToSquares :: Move -> [Square] -> MoveList -> MoveList
recurGeneratePawnMovesFromToSquares _ [] !result = result
recurGeneratePawnMovesFromToSquares !mask !toSquares !result = recurGeneratePawnMovesFromToSquares mask (tail toSquares) newResult
  where thisToSquare = head toSquares
        baseMove = (.|.) mask thisToSquare
        newResult = if thisToSquare >= 56 || thisToSquare <= 7
                    then promotionMoves baseMove `DList.append` result
                    else DList.singleton baseMove `DList.append` result

generatePawnMoves :: Position -> MoveList
generatePawnMoves !position = recurGeneratePawnMoves (bitRefList bitboard) position forwardPawnMoves capturePawnMoves (emptySquaresBitboard position) bitboard DList.empty
  where bitboard = bitboardForMover position Pawn
        forwardPawnMoves = if mover position == White then whitePawnMovesForward else blackPawnMovesForward
        capturePawnMoves = if mover position == White then whitePawnMovesCapture else blackPawnMovesCapture

recurGeneratePawnMoves :: [Square] -> Position -> BitboardArray -> BitboardArray -> Bitboard -> Bitboard -> MoveList -> MoveList
recurGeneratePawnMoves [] _ _ _ _ _ !result = result
recurGeneratePawnMoves !fromSquares !position !forwardPawnMoves !capturePawnMoves !emptySquares !moverPawns !result =
  recurGeneratePawnMoves (tail fromSquares) position forwardPawnMoves capturePawnMoves emptySquares moverPawns (result `DList.append` thisResult)
  where fromSquare = head fromSquares
        pawnForwardAndCaptureMoves = pawnForwardAndCaptureMovesBitboard fromSquare capturePawnMoves (pawnForwardMovesBitboard ((.&.) (forwardPawnMoves V.! fromSquare) emptySquares) position) position
        thisResult = generatePawnMovesFromToSquares fromSquare (bitRefList pawnForwardAndCaptureMoves)

pawnForwardMovesBitboard :: Bitboard -> Position -> Bitboard
pawnForwardMovesBitboard !pawnMoves !position = (.|.) pawnMoves ((.&.) (potentialPawnJumpMoves pawnMoves position) (emptySquaresBitboard position))

enPassantCaptureRank :: Mover -> Bitboard
enPassantCaptureRank White = rank6Bits
enPassantCaptureRank Black = rank3Bits

pawnForwardAndCaptureMovesBitboard :: Square -> BitboardArray -> Bitboard -> Position -> Bitboard
pawnForwardAndCaptureMovesBitboard !fromSquare !capturePawnMoves !nonCaptures !position = (.|.) nonCaptures captures
  where eps = enPassantSquare position
        captures = if eps /= enPassantNotAvailable && (.&.) (bit eps) (enPassantCaptureRank (mover position)) /= 0
                  then pawnCapturesPlusEnPassantSquare capturePawnMoves fromSquare position
                  else pawnCaptures capturePawnMoves fromSquare (enemyBitboard position)

pawnCapturesPlusEnPassantSquare :: BitboardArray -> Square -> Position -> Bitboard
pawnCapturesPlusEnPassantSquare !bs !square !position = pawnCaptures bs square (enemyBitboard position .|. (if eps == enPassantNotAvailable then 0 else bit eps)) where eps = enPassantSquare position

pawnCaptures :: BitboardArray -> Square -> Bitboard -> Bitboard
pawnCaptures !captureMask !square = (.&.) (captureMask V.! square)

potentialPawnJumpMoves :: Bitboard -> Position -> Bitboard
potentialPawnJumpMoves !bb !position = if mover position == White then (.&.) (bb `shiftL` 8) rank4Bits else (.&.) (bb `shiftR` 8) rank5Bits

generateCastleMoves :: Position -> MoveList
generateCastleMoves !position = if mover position == White
    then generateCastleMovesForMover position 3 4 Black (whiteKingCastleAvailable castlePrivs) (whiteQueenCastleAvailable castlePrivs) emptyCastleSquaresWhiteKing emptyCastleSquaresWhiteQueen noCheckCastleSquaresWhiteKing noCheckCastleSquaresWhiteQueen allPieces
    else generateCastleMovesForMover position 59 60 White (blackKingCastleAvailable castlePrivs) (blackQueenCastleAvailable castlePrivs) emptyCastleSquaresBlackKing emptyCastleSquaresBlackQueen noCheckCastleSquaresBlackKing noCheckCastleSquaresBlackQueen allPieces
  where castlePrivs = positionCastlePrivs position
        allPieces = allPiecesBitboard position

generateCastleMovesForMover :: Position -> Square -> Square -> Mover -> Bool -> Bool -> Bitboard -> Bitboard -> Bitboard -> Bitboard -> Bitboard -> MoveList
generateCastleMovesForMover !position !kingStartSquare !queenStartSquare !opponent !canKing !canQueen !kingSpaces !queenSpaces !noCheckKingSide !noCheckQueenSide !allPieces =
  DList.fromList ([(.|.) (fromSquareMask kingStartSquare) ((-) kingStartSquare 2) :: Move | canKing && (.&.) allPieces kingSpaces == 0 && not (anySquaresInBitboardAttacked position opponent noCheckKingSide)]) `DList.append`
  DList.fromList ([(.|.) (fromSquareMask kingStartSquare) ((+) kingStartSquare 2) :: Move | canQueen && (.&.) allPieces queenSpaces == 0 && not (anySquaresInBitboardAttacked position opponent noCheckQueenSide)])

anySquaresInBitboardAttacked :: Position -> Mover -> Bitboard -> Bool
anySquaresInBitboardAttacked position attacker bitboard = any (\x -> isSquareAttackedBy position x attacker) (bitRefList bitboard)

pawnMovesCaptureOfColour :: Mover -> BitboardArray
pawnMovesCaptureOfColour !mover = if mover == White then whitePawnMovesCapture else blackPawnMovesCapture

kingSquare :: Position -> Mover -> Square
kingSquare !position !colour = if colour == White
    then head (bitRefList (whiteKingBitboard bb))
    else head (bitRefList (blackKingBitboard bb))
  where bb = position

isCheck :: Position -> Mover -> Bool
isCheck !position !colour = isSquareAttackedBy position (kingSquare position colour) (if colour == White then Black else White)

magicIndexForPiece :: Piece -> Square -> Bitboard -> Int
magicIndexForPiece !piece !pieceSquare !allPieceBitboard = fromIntegral (shiftR rawIndex shiftMagic) :: Int
    where magicVars = if piece == Rook then magicRookVars else magicBishopVars
          numberMagic = magicNumber magicVars V.! pieceSquare
          shiftMagic = magicNumberShifts magicVars V.!pieceSquare
          maskMagic = occupancyMask magicVars V.! pieceSquare
          occupancy = (.&.) allPieceBitboard maskMagic
          rawIndex = fromIntegral(occupancy * numberMagic) :: Word

rookMovePiecesBitboard :: Position -> Mover -> Bitboard
rookMovePiecesBitboard !position !mover = if mover == White
    then (.|.) (whiteRookBitboard pb) (whiteQueenBitboard pb)
    else (.|.) (blackRookBitboard pb) (blackQueenBitboard pb)
  where pb = position

bishopMovePiecesBitboard :: Position -> Mover -> Bitboard
bishopMovePiecesBitboard !position !mover = if mover == White
    then (.|.) (whiteBishopBitboard pb) (whiteQueenBitboard pb)
    else (.|.) (blackBishopBitboard pb) (blackQueenBitboard pb)
  where pb = position

isSquareAttackedByKnight :: Position -> Square -> Mover -> Bool
isSquareAttackedByKnight !position !attackedSquare !attacker = (.&.) knightBitboard (knightMovesBitboards V.! attackedSquare) /= 0
    where knightBitboard = (if attacker == White then whiteKnightBitboard else blackKnightBitboard) pb
          pb = position

isSquareAttackedByKing :: Position -> Square -> Mover -> Bool
isSquareAttackedByKing !position !attackedSquare !attacker = (.&.) kingBitboard (kingMovesBitboards V.! attackedSquare) /= 0
    where kingBitboard = (if attacker == White then whiteKingBitboard else blackKingBitboard) pb
          pb = position

isSquareAttackedByPawn :: Position -> Square -> Mover -> Bool
isSquareAttackedByPawn !position !attackedSquare !attacker = (.&.) pawnBitboard (pawnMovesCaptureOfColour defenderColour V.! attackedSquare) /= 0
    where pawnBitboard = (if attacker == White then whitePawnBitboard else blackPawnBitboard) pb
          pb = position
          defenderColour = if attacker == White then Black else White

isSquareAttackedByBishop :: Position -> Square -> Mover -> Bool
isSquareAttackedByBishop !position !attackedSquare !attacker = any (\x -> isBishopAttackingSquare attackedSquare x apb) (bitRefList (bishopMovePiecesBitboard position attacker))
    where apb = allPiecesBitboard position

isSquareAttackedByRook :: Position -> Square -> Mover -> Bool
isSquareAttackedByRook !position !attackedSquare !attacker = any (\x -> isRookAttackingSquare attackedSquare x apb) (bitRefList (rookMovePiecesBitboard position attacker))
    where apb = allPiecesBitboard position

isBishopAttackingSquare :: Square -> Square -> Bitboard -> Bool
isBishopAttackingSquare !attackedSquare !pieceSquare allPieceBitboard =
  (.&.) ((magicMoves magicBishopVars ! pieceSquare) V.! magicIndexForPiece Bishop pieceSquare allPieceBitboard) (bit attackedSquare) /= 0

isRookAttackingSquare :: Square -> Square -> Bitboard -> Bool
isRookAttackingSquare !attackedSquare !pieceSquare !allPieceBitboard =
  (.&.) ((magicMoves magicRookVars ! pieceSquare) V.! magicIndexForPiece Rook pieceSquare allPieceBitboard) (bit attackedSquare) /= 0
  
isSquareAttackedBy :: Position -> Square -> Mover -> Bool
isSquareAttackedBy !position !attackedSquare !attacker =
  isSquareAttackedByPawn position attackedSquare attacker ||
  isSquareAttackedByKnight position attackedSquare attacker ||
  isSquareAttackedByKing position attackedSquare attacker ||
  isSquareAttackedByRook position attackedSquare attacker ||
  isSquareAttackedByBishop position attackedSquare attacker

moves :: Position -> [Move]
moves !position = DList.toList
  ( generatePawnMoves position `DList.append`
    generateKnightMoves position `DList.append`
    generateBishopMoves position `DList.append`
    generateRookMoves position `DList.append`
    generateKingMoves position `DList.append`
    generateCastleMoves position )

