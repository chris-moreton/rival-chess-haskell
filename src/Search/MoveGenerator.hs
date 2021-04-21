{-# LANGUAGE StrictData #-}

module Search.MoveGenerator where
  
import Types
import Data.Word
import Data.Bits
import Util.Bitboards
import Util.MagicBitboards
import Util.Utils
import Search.MoveConstants
import Data.Array.IArray

bitboardForMover :: Position -> Piece -> Bitboard
bitboardForMover position = bitboardForColour (positionBitboards position) (mover position)

bitboardForColour :: PieceBitboards -> Mover -> Piece -> Bitboard
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
bitRefList bitboard = recurBitRefList bitboard []

recurBitRefList :: Bitboard -> [Square] -> [Square]
recurBitRefList 0 result = result
recurBitRefList bitboard result = do
  let square = countTrailingZeros bitboard
  recurBitRefList (xor bitboard (bit square)) (result ++ [square])

bitString :: Bitboard -> String
bitString bitboard = recurBitString bitboard 63 ""
  
recurBitString :: Bitboard -> Int -> String -> String
recurBitString _ (-1) result = result
recurBitString bitboard square result = do
  let bitMask = bit square
  recurBitString (xor bitboard bitMask) (square - 1) (result ++ if bitMask == (.&.) bitMask bitboard then "1" else "0")

allBitsExceptFriendlyPieces :: Position -> Bitboard
allBitsExceptFriendlyPieces position = complement (foldl (.|.) 0 (bitboardListForColour position (mover position)))

movesFromToSquares :: Square -> [Square] -> [Move]
movesFromToSquares fromSquare toSquares = recurMovesFromToSquares fromSquare toSquares []

recurMovesFromToSquares :: Square -> [Square] -> [Move] -> [Move]
recurMovesFromToSquares _ [] result = result
recurMovesFromToSquares fromSquare toSquares result = do
  let shiftedFrom = shiftL fromSquare 16
  recurMovesFromToSquares fromSquare (tail toSquares) (result ++ [(.|.) shiftedFrom (head toSquares)])

generateKnightMoves :: Position -> [Move]
generateKnightMoves position = do
  let bitboard = bitboardForMover position Knight
  let fromSquares = bitRefList bitboard
  recurKnightMoves position fromSquares []

recurKnightMoves :: Position -> [Square] -> [Move] -> [Move]
recurKnightMoves _ [] result = result
recurKnightMoves position fromSquares result = do
  let fromSquare = head fromSquares
  let toSquares = bitRefList ((.&.) (knightMovesBitboards!!fromSquare) (allBitsExceptFriendlyPieces position))
  recurKnightMoves position (tail fromSquares) (result ++ movesFromToSquares fromSquare toSquares)

generateKingMoves :: Position -> [Move]
generateKingMoves position = do
  let kingSquare = countTrailingZeros (bitboardForMover position King)
  let toSquares = bitRefList ((.&.) (kingMovesBitboards!!kingSquare) (allBitsExceptFriendlyPieces position))
  movesFromToSquares kingSquare toSquares

generateBishopMoves :: Position -> [Move]
generateBishopMoves position = generateSliderMoves position Bishop

generateRookMoves :: Position -> [Move]
generateRookMoves position = generateSliderMoves position Rook

generateSliderMoves :: Position -> Piece -> [Move]
generateSliderMoves position piece = do
  let bitboards = positionBitboards position
  let magicVars = if piece == Bishop then magicBishopVars else magicRookVars
  let thisMover = mover position
  let bitboard = (.|.) (bitboardForColour bitboards thisMover piece) (bitboardForColour bitboards thisMover Queen)
  let fromSquares = bitRefList bitboard
  recurGenerateSliderMoves fromSquares position magicVars []

recurGenerateSliderMoves :: [Square] -> Position -> MagicVars -> [Move] -> [Move]
recurGenerateSliderMoves [] _ _ result = result
recurGenerateSliderMoves fromSquares position magicVars result = do
  let fromSquare = head fromSquares

  let moveMagic = magicMoves magicVars!fromSquare
  let numberMagic = magicNumber magicVars!fromSquare
  let shiftMagic = magicNumberShifts magicVars!fromSquare
  let maskMagic = occupancyMask magicVars!fromSquare

  let occupancy = (.&.) (allPiecesBitboard position) maskMagic
  let rawIndex = fromIntegral(occupancy * numberMagic) :: Word

  let toSquaresMagicIndex = fromIntegral(shiftR rawIndex shiftMagic) :: Int
  let toSquaresBitboard = (.&.) (moveMagic!toSquaresMagicIndex) (allBitsExceptFriendlyPieces position)

  let toSquares = bitRefList toSquaresBitboard

  let thisResult = recurGenerateSliderMovesWithToSquares fromSquare toSquares []
  recurGenerateSliderMoves (tail fromSquares) position magicVars (result ++ thisResult)

recurGenerateSliderMovesWithToSquares :: Square -> [Square] -> [Move] -> [Move]
recurGenerateSliderMovesWithToSquares fromSquare [] result = result
recurGenerateSliderMovesWithToSquares fromSquare toSquares result =
  recurGenerateSliderMovesWithToSquares fromSquare (tail toSquares) (result ++ [(.|.) (fromSquareMask fromSquare) (head toSquares)])

promotionMoves :: Move -> [Move]
promotionMoves move = [
    (.|.) move promotionQueenMoveMask
  , (.|.) move promotionRookMoveMask
  , (.|.) move promotionBishopMoveMask
  , (.|.) move promotionKnightMoveMask]

generatePawnMovesFromToSquares :: Square -> [Square] -> [Move]
generatePawnMovesFromToSquares fromSquare toSquares = do
  let mask = fromSquareMask fromSquare
  recurGeneratePawnMovesFromToSquares mask toSquares []

recurGeneratePawnMovesFromToSquares :: MoveMask -> [Square] -> [Move] -> [Move]
recurGeneratePawnMovesFromToSquares _ [] result = result
recurGeneratePawnMovesFromToSquares mask toSquares result = do
  let thisToSquare = head toSquares
  let baseMove = (.|.) mask thisToSquare
  let newResult = if thisToSquare >= 56 || thisToSquare <= 7 then promotionMoves baseMove else [baseMove]
  recurGeneratePawnMovesFromToSquares mask (tail toSquares) newResult ++ result

generatePawnMoves :: Position -> [Move]
generatePawnMoves position = do
  let bitboard = bitboardForMover position Pawn
  let forwardPawnMoves = if mover position == White then whitePawnMovesForward else blackPawnMovesForward
  let capturePawnMoves = if mover position == White then whitePawnMovesCapture else blackPawnMovesCapture
  recurGeneratePawnMoves (bitRefList bitboard) position forwardPawnMoves capturePawnMoves (emptySquaresBitboard position) bitboard []

recurGeneratePawnMoves :: [Square] -> Position -> [Bitboard] -> [Bitboard] -> Bitboard -> Bitboard -> [Move] -> [Move]
recurGeneratePawnMoves [] _ _ _ _ _ result = result
recurGeneratePawnMoves fromSquares position forwardPawnMoves capturePawnMoves emptySquares moverPawns result = do
  let fromSquare = head fromSquares
  let pawnForwardAndCaptureMoves = pawnForwardAndCaptureMovesBitboard fromSquare capturePawnMoves (pawnForwardMovesBitboard ((.&.) (forwardPawnMoves!!fromSquare) emptySquares) position) position
  let thisResult = generatePawnMovesFromToSquares fromSquare (bitRefList pawnForwardAndCaptureMoves)
  recurGeneratePawnMoves (tail fromSquares) position forwardPawnMoves capturePawnMoves emptySquares moverPawns (result ++ thisResult)

pawnForwardMovesBitboard :: Bitboard -> Position -> Bitboard
pawnForwardMovesBitboard pawnMoves position = (.|.) pawnMoves ((.&.) (potentialPawnJumpMoves pawnMoves position) (emptySquaresBitboard position))

enPassantCaptureRank :: Mover -> Bitboard
enPassantCaptureRank mover = if mover == White then rank6Bits else rank3Bits

pawnForwardAndCaptureMovesBitboard :: Square -> [Bitboard] -> Bitboard -> Position -> Bitboard
pawnForwardAndCaptureMovesBitboard fromSquare capturePawnMoves nonCaptures position = do
  let eps = enPassantSquare position
  let captures = if eps /= enPassantNotAvailable && (.&.) (bit eps) (enPassantCaptureRank (mover position)) /= 0
                  then pawnCapturesPlusEnPassantSquare capturePawnMoves fromSquare position
                  else pawnCaptures capturePawnMoves fromSquare (enemyBitboard position)
  (.|.) nonCaptures captures

pawnCapturesPlusEnPassantSquare :: [Bitboard] -> Square -> Position -> Bitboard
pawnCapturesPlusEnPassantSquare bs square position = pawnCaptures bs square (enemyBitboard position .|. (if eps == enPassantNotAvailable then 0 else bit eps)) where eps = enPassantSquare position

pawnCaptures :: [Bitboard] -> Square -> Bitboard -> Bitboard
pawnCaptures captureMask square = (.&.) (captureMask !! square)

potentialPawnJumpMoves :: Bitboard -> Position -> Bitboard
potentialPawnJumpMoves bb position = if mover position == White then (.&.) (bb `shiftL` 8) rank4Bits else (.&.) (bb `shiftR` 8) rank5Bits

generateCastleMoves :: Position -> [Move]
generateCastleMoves position = do
  let castlePrivs = positionCastlePrivs position
  let allPieces = allPiecesBitboard position
  if mover position == White
    then generateCastleMovesForMover position 3 4 Black (whiteKingCastleAvailable castlePrivs) (whiteQueenCastleAvailable castlePrivs) emptyCastleSquaresWhiteKing emptyCastleSquaresWhiteQueen noCheckCastleSquaresWhiteKing noCheckCastleSquaresWhiteQueen allPieces
    else generateCastleMovesForMover position 59 60 White (blackKingCastleAvailable castlePrivs) (blackQueenCastleAvailable castlePrivs) emptyCastleSquaresBlackKing emptyCastleSquaresBlackQueen noCheckCastleSquaresBlackKing noCheckCastleSquaresBlackQueen allPieces

generateCastleMovesForMover :: Position -> Square -> Square -> Mover -> Bool -> Bool -> Bitboard -> Bitboard -> Bitboard -> Bitboard -> Bitboard -> [Move]
generateCastleMovesForMover position kingStartSquare queenStartSquare opponent canKing canQueen kingSpaces queenSpaces noCheckKingSide noCheckQueenSide allPieces =
  ([(.|.) (fromSquareMask kingStartSquare) ((-) kingStartSquare 2) | canKing && ((.&.) allPieces kingSpaces == 0) && not (anySquaresInBitboardAttacked position opponent noCheckKingSide)]) ++
   [(.|.) (fromSquareMask kingStartSquare) ((+) kingStartSquare 2) | canQueen && (.&.) allPieces queenSpaces == 0 && not (anySquaresInBitboardAttacked position opponent noCheckQueenSide)]

anySquaresInBitboardAttacked :: Position -> Mover -> Bitboard -> Bool
anySquaresInBitboardAttacked position attacker bitboard = any (\x -> isSquareAttackedBy position x attacker) (bitRefList bitboard)

pawnMovesCaptureOfColour :: Mover -> [Bitboard]
pawnMovesCaptureOfColour mover = if mover == White then whitePawnMovesCapture else blackPawnMovesCapture

kingSquare :: Position -> Mover -> Square
kingSquare position colour = do
  let bb = positionBitboards position
  if colour == White
    then head (bitRefList (whiteKingBitboard bb))
    else head (bitRefList (blackKingBitboard bb))

isCheck :: Position -> Mover -> Bool
isCheck position colour = isSquareAttackedBy position (kingSquare position colour) (if colour == White then Black else White)

isBishopAttackingSquare :: Square -> Square -> Bitboard -> Bool
isBishopAttackingSquare attackedSquare pieceSquare allPieceBitboard =
  (.&.) ((magicMoves magicBishopVars ! pieceSquare) ! magicIndexForPiece Bishop pieceSquare allPieceBitboard) (1 `shiftL` attackedSquare) /= 0

isRookAttackingSquare :: Square -> Square -> Bitboard -> Bool
isRookAttackingSquare attackedSquare pieceSquare allPieceBitboard =
  (.&.) ((magicMoves magicRookVars ! pieceSquare) ! magicIndexForPiece Rook pieceSquare allPieceBitboard) (1 `shiftL` attackedSquare) /= 0

magicIndexForPiece :: Piece -> Square -> Bitboard -> Int
magicIndexForPiece piece pieceSquare allPieceBitboard = do
    let magicVars = if piece == Rook then magicRookVars else magicBishopVars
    let moveMagic = magicMoves magicVars!pieceSquare
    let numberMagic = magicNumber magicVars!pieceSquare
    let shiftMagic = magicNumberShifts magicVars!pieceSquare
    let maskMagic = occupancyMask magicVars!pieceSquare
    let occupancy = (.&.) allPieceBitboard maskMagic
    let rawIndex = fromIntegral(occupancy * numberMagic) :: Word
    fromIntegral(shiftR rawIndex shiftMagic) :: Int

rookMovePiecesBitboard :: Position -> Mover -> Bitboard
rookMovePiecesBitboard position mover = do
  let pb = positionBitboards position
  if mover == White
    then (.|.) (whiteRookBitboard pb) (whiteQueenBitboard pb)
    else (.|.) (blackRookBitboard pb) (blackQueenBitboard pb)

bishopMovePiecesBitboard :: Position -> Mover -> Bitboard
bishopMovePiecesBitboard position mover = do
  let pb = positionBitboards position
  if mover == White
    then (.|.) (whiteBishopBitboard pb) (whiteQueenBitboard pb)
    else (.|.) (blackBishopBitboard pb) (blackQueenBitboard pb)

isSquareAttackedBy :: Position -> Square -> Mover -> Bool
isSquareAttackedBy position attackedSquare attacker = do
  let pb = positionBitboards position
  let apb = allPiecesBitboard position
  let defenderColour = if attacker == White then Black else White
  let knightBitboard = if attacker == White then whiteKnightBitboard pb else blackKnightBitboard pb
  let kingBitboard = if attacker == White then whiteKingBitboard pb else blackKingBitboard pb
  let pawnBitboard = if attacker == White then whitePawnBitboard pb else blackPawnBitboard pb
  let pawnAttack = (.&.) pawnBitboard (pawnMovesCaptureOfColour defenderColour !! attackedSquare) /= 0
  let knightAttack = (.&.) knightBitboard (knightMovesBitboards!!attackedSquare) /= 0
  let kingAttack = (.&.) kingBitboard (kingMovesBitboards!!attackedSquare) /= 0
  let bishopAttack = any (\x -> isBishopAttackingSquare attackedSquare x apb) (bitRefList (bishopMovePiecesBitboard position attacker))
  let rookAttack = any (\x -> isRookAttackingSquare attackedSquare x apb) (bitRefList (rookMovePiecesBitboard position attacker))
  pawnAttack || knightAttack || kingAttack || bishopAttack || rookAttack

moves :: Position -> [Move]
moves position =
  generatePawnMoves position ++
  generateKnightMoves position ++
  generateBishopMoves position ++
  generateRookMoves position ++
  generateKingMoves position ++
  generateCastleMoves position


