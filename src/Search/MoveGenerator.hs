module Search.MoveGenerator where
  
import Types
import Data.Word
import Data.Bits
import Util.Bitboards

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

allPiecesBitboard :: Position -> Bitboard
allPiecesBitboard position = foldl (.|.) 0 (bitboardListForColour position White ++ bitboardListForColour position Black)

movesFromToSquares :: Square -> [Square] -> [CompactMove]
movesFromToSquares fromSquare toSquares = recurMovesFromToSquares fromSquare toSquares []

recurMovesFromToSquares :: Square -> [Square] -> [CompactMove] -> [CompactMove]
recurMovesFromToSquares fromSquare [] result = result
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