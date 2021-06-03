{-# LANGUAGE DeriveGeneric,BangPatterns,DeriveAnyClass,KindSignatures #-}

module Types where

import GHC.Generics ( Generic )
import Data.Bits ( Bits((.|.)) )
import Alias ( Bitboard, Square )
import Control.DeepSeq ( NFData )
    
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
} deriving (Generic,Show,NFData)

instance Eq Position where
    a == b = whitePawnBitboard a == whitePawnBitboard b &&
      whiteKnightBitboard a == whiteKnightBitboard b &&
      whiteBishopBitboard a == whiteBishopBitboard b &&
      whiteQueenBitboard a == whiteQueenBitboard b &&
      whiteKingBitboard a == whiteKingBitboard b &&
      whiteRookBitboard a == whiteRookBitboard b &&
      blackPawnBitboard a == blackPawnBitboard b &&
      blackKnightBitboard a == blackKnightBitboard b &&
      blackBishopBitboard a == blackBishopBitboard b &&
      blackQueenBitboard a == blackQueenBitboard b &&
      blackKingBitboard a == blackKingBitboard b &&
      blackPiecesBitboard a == blackPiecesBitboard b &&
      enPassantSquare a == enPassantSquare b &&
      whiteKingCastleAvailable a == whiteKingCastleAvailable b &&
      whiteQueenCastleAvailable a == whiteQueenCastleAvailable b &&
      blackKingCastleAvailable a == blackKingCastleAvailable b &&
      blackQueenCastleAvailable a == blackQueenCastleAvailable b &&
      mover a == mover b

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
