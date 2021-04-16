module Search.MoveGenerator where
  
import Types
import Data.Word
import Data.Bits
import Util.Bitboards
import Util.MagicBitboards
import Search.MoveConstants

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

fromSquareMask :: Square -> MoveMask
fromSquareMask sq = sq `shiftL` 16

bitRefList :: Bitboard -> [BitRef]
bitRefList bitboard = recurBitRefList bitboard []

recurBitRefList :: Bitboard -> [BitRef] -> [BitRef]
recurBitRefList 0 result = result
recurBitRefList bitboard result = do
  let square = countTrailingZeros bitboard
  let bitMask = shiftL 1 square
  recurBitRefList (xor bitboard bitMask) (result ++ [square])

bitString :: Bitboard -> String
bitString bitboard = recurBitString bitboard 63 ""
  
recurBitString :: Bitboard -> Int -> String -> String
recurBitString _ (-1) result = result
recurBitString bitboard bitRef result = do
  let bitMask = shiftL 1 bitRef
  recurBitString (xor bitboard bitMask) (bitRef - 1) (result ++ if bitMask == (.&.) bitMask bitboard then "1" else "0")
        
bitboardListForColour :: Position -> Mover -> [Bitboard]
bitboardListForColour position colour = do
  let bitboards = positionBitboards position
  if colour == White
  then
    [whitePawnBitboard bitboards,whiteKnightBitboard bitboards,whiteKingBitboard bitboards,whiteBishopBitboard bitboards,whiteQueenBitboard bitboards,whiteRookBitboard bitboards]
  else
    [blackPawnBitboard bitboards,blackKnightBitboard bitboards,blackKingBitboard bitboards,blackBishopBitboard bitboards,blackQueenBitboard bitboards,blackRookBitboard bitboards]
          
allBitsExceptFriendlyPieces :: Position -> Bitboard
allBitsExceptFriendlyPieces position = complement (foldl (.|.) 0 (bitboardListForColour position (mover position)))

opponent :: Position -> Mover
opponent position = if mover position == White then Black else White

enemyBitboard :: Position -> Bitboard
enemyBitboard position = foldl (.|.) 0 (bitboardListForColour position (opponent position))

allPiecesBitboard :: Position -> Bitboard
allPiecesBitboard position = foldl (.|.) 0 (bitboardListForColour position White ++ bitboardListForColour position Black)

emptySquaresBitboard :: Position -> Bitboard
emptySquaresBitboard position = complement (allPiecesBitboard position)

movesFromToSquares :: Square -> [Square] -> [CompactMove]
movesFromToSquares fromSquare toSquares = recurMovesFromToSquares fromSquare toSquares []

recurMovesFromToSquares :: Square -> [Square] -> [CompactMove] -> [CompactMove]
recurMovesFromToSquares _ [] result = result
recurMovesFromToSquares fromSquare toSquares result = do
  let shiftedFrom = shiftL fromSquare 16
  recurMovesFromToSquares fromSquare (tail toSquares) (result ++ [(.|.) shiftedFrom (head toSquares)])

generateKnightMoves :: Position -> [CompactMove]
generateKnightMoves position = do
  let bitboard = bitboardForMover position Knight
  let fromSquares = bitRefList bitboard
  recurKnightMoves position fromSquares []

recurKnightMoves :: Position -> [BitRef] -> [CompactMove] -> [CompactMove]
recurKnightMoves _ [] result = result
recurKnightMoves position fromSquares result = do
  let fromSquare = head fromSquares
  let toSquares = bitRefList ((.&.) (knightMovesBitboards!!fromSquare) (allBitsExceptFriendlyPieces position))
  recurKnightMoves position (tail fromSquares) (result ++ movesFromToSquares fromSquare toSquares)

generateKingMoves :: Position -> [CompactMove]
generateKingMoves position = do
  let kingSquare = countTrailingZeros (bitboardForMover position King)
  let toSquares = bitRefList ((.&.) (kingMovesBitboards!!kingSquare) (allBitsExceptFriendlyPieces position))
  movesFromToSquares kingSquare toSquares

generateBishopMoves :: Position -> [CompactMove]
generateBishopMoves position = generateSliderMoves position Bishop

generateRookMoves :: Position -> [CompactMove]
generateRookMoves position = generateSliderMoves position Rook

generateSliderMoves :: Position -> Piece -> [CompactMove]
generateSliderMoves position piece = do
  let bitboards = positionBitboards position
  let magicVars = if piece == Bishop then magicBishopVars else magicRookVars
  let thisMover = mover position
  let bitboard = (.|.) (bitboardForColour bitboards thisMover piece) (bitboardForColour bitboards thisMover Queen)
  let fromSquares = bitRefList bitboard
  recurGenerateSliderMoves fromSquares position magicVars []

recurGenerateSliderMoves :: [Square] -> Position -> MagicVars -> [CompactMove] -> [CompactMove]
recurGenerateSliderMoves [] _ _ result = result
recurGenerateSliderMoves fromSquares position magicVars result = do
  let fromSquare = head fromSquares

  let moveMagic = magicMoves magicVars!!fromSquare
  let numberMagic = magicNumber magicVars!!fromSquare
  let shiftMagic = magicNumberShifts magicVars!!fromSquare
  let maskMagic = occupancyMask magicVars!!fromSquare

  let occupancy = (.&.) (allPiecesBitboard position) maskMagic
  let rawIndex = fromIntegral(occupancy * numberMagic) :: Word

  let toSquaresMagicIndex = fromIntegral(shiftR rawIndex shiftMagic) :: Int
  let toSquaresBitboard = (.&.) (moveMagic!!toSquaresMagicIndex) (allBitsExceptFriendlyPieces position)

  let toSquares = bitRefList toSquaresBitboard

  let thisResult = recurGenerateSliderMovesWithToSquares fromSquare toSquares []
  recurGenerateSliderMoves (tail fromSquares) position magicVars (result ++ thisResult)

recurGenerateSliderMovesWithToSquares :: Square -> [Square] -> [CompactMove] -> [CompactMove]
recurGenerateSliderMovesWithToSquares fromSquare [] result = result
recurGenerateSliderMovesWithToSquares fromSquare toSquares result =
  recurGenerateSliderMovesWithToSquares fromSquare (tail toSquares) (result ++ [(.|.) (fromSquareMask fromSquare) (head toSquares)])

promotionMoves :: CompactMove -> [CompactMove]
promotionMoves move = [
    (.|.) move promotionQueenMoveMask
  , (.|.) move promotionRookMoveMask
  , (.|.) move promotionBishopMoveMask
  , (.|.) move promotionKnightMoveMask]

generatePawnMovesFromToSquares :: Square -> [Square] -> [CompactMove]
generatePawnMovesFromToSquares fromSquare toSquares = do
  let mask = fromSquareMask fromSquare
  recurGeneratePawnMovesFromToSquares mask toSquares []

recurGeneratePawnMovesFromToSquares :: MoveMask -> [Square] -> [CompactMove] -> [CompactMove]
recurGeneratePawnMovesFromToSquares _ [] result = result
recurGeneratePawnMovesFromToSquares mask toSquares result = do
  let thisToSquare = head toSquares
  let baseMove = (.|.) mask thisToSquare
  let newResult = if thisToSquare >= 56 || thisToSquare <= 7 then promotionMoves baseMove else [baseMove]
  recurGeneratePawnMovesFromToSquares mask (tail toSquares) newResult ++ result

generatePawnMoves :: Position -> [CompactMove]
generatePawnMoves position = do
  let bitboard = bitboardForMover position Pawn
  let forwardPawnMoves = if mover position == White then whitePawnMovesForward else blackPawnMovesForward
  let capturePawnMoves = if mover position == White then whitePawnMovesCapture else blackPawnMovesCapture
  recurGeneratePawnMoves (bitRefList bitboard) position forwardPawnMoves capturePawnMoves (emptySquaresBitboard position) bitboard []

recurGeneratePawnMoves :: [Square] -> Position -> [Bitboard] -> [Bitboard] -> Bitboard -> Bitboard -> [CompactMove] -> [CompactMove]
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
  let captures = if eps > -1 && (.&.) (1 `shiftL` eps) (enPassantCaptureRank (mover position)) /= 0
                  then pawnCapturesPlusEnPassantSquare capturePawnMoves fromSquare position
                  else pawnCaptures capturePawnMoves fromSquare (enemyBitboard position)
  (.|.) nonCaptures captures

pawnCapturesPlusEnPassantSquare :: [Bitboard] -> Square -> Position -> Bitboard
pawnCapturesPlusEnPassantSquare bs square position = (.|.) (pawnCaptures bs square (enemyBitboard position)) (pawnCaptures bs square (1 `shiftL` enPassantSquare position))

pawnCaptures :: [Bitboard] -> Square -> Bitboard -> Bitboard
pawnCaptures captureMask square = (.&.) (captureMask !! square)

potentialPawnJumpMoves :: Bitboard -> Position -> Bitboard
potentialPawnJumpMoves bb position = if mover position == White then (.&.) (bb `shiftL` 8) rank4Bits else (.&.) (bb `shiftR` 8) rank5Bits