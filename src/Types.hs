{-# LANGUAGE DeriveGeneric,BangPatterns,DeriveAnyClass #-}

module Types where

import GHC.Generics
import Data.Bits
import Alias
import Control.DeepSeq

data Mover = White | Black deriving (Enum,Show,Eq,NFData,Generic)
data Piece = Pawn | King | Queen | Bishop | Knight | Rook deriving (Enum,Show,Eq)

data Position = Position {
    whitePawnBitboard :: {-# UNPACK #-} !Bitboard
  , whiteKnightBitboard :: {-# UNPACK #-} !Bitboard
  , whiteBishopBitboard :: {-# UNPACK #-} !Bitboard
  , whiteQueenBitboard :: {-# UNPACK #-} !Bitboard
  , whiteKingBitboard :: {-# UNPACK #-} !Bitboard
  , whiteRookBitboard :: {-# UNPACK #-} !Bitboard
  , blackPawnBitboard :: {-# UNPACK #-} !Bitboard
  , blackKnightBitboard :: {-# UNPACK #-} !Bitboard
  , blackBishopBitboard :: {-# UNPACK #-} !Bitboard
  , blackQueenBitboard :: {-# UNPACK #-} !Bitboard
  , blackKingBitboard :: {-# UNPACK #-} !Bitboard
  , blackRookBitboard :: {-# UNPACK #-} !Bitboard
  , allPiecesBitboard :: {-# UNPACK #-} !Bitboard
  , whitePiecesBitboard :: {-# UNPACK #-} !Bitboard
  , blackPiecesBitboard :: {-# UNPACK #-} !Bitboard
  , mover :: !Mover
  , enPassantSquare :: {-# UNPACK #-} !Square
  , whiteKingCastleAvailable  :: !Bool
  , blackKingCastleAvailable  :: !Bool
  , whiteQueenCastleAvailable :: !Bool
  , blackQueenCastleAvailable :: !Bool
  , halfMoves :: Int
  , moveNumber :: Int
} deriving (Generic,Show,Eq,NFData)

{-# INLINE bitboardForMover #-}
bitboardForMover :: Position -> Piece -> Bitboard
bitboardForMover !position = bitboardForColour position (mover position)

{-# INLINE bitboardForColour #-}
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

{-# INLINE sliderBitboardForColour #-}
sliderBitboardForColour :: Position -> Mover -> Piece -> Bitboard
sliderBitboardForColour !pieceBitboards White Rook = whiteRookBitboard pieceBitboards .|. whiteQueenBitboard pieceBitboards
sliderBitboardForColour !pieceBitboards White Bishop = whiteBishopBitboard pieceBitboards .|. whiteQueenBitboard pieceBitboards
sliderBitboardForColour !pieceBitboards Black Rook = blackRookBitboard pieceBitboards .|. blackQueenBitboard pieceBitboards
sliderBitboardForColour !pieceBitboards Black Bishop = blackBishopBitboard pieceBitboards .|. blackQueenBitboard pieceBitboards
