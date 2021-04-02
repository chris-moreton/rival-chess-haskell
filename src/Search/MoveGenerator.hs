module Search.MoveGenerator where
  
import Types

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
